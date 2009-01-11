unit TestMiscFunctions;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestMiscProcess, released Jan 2007.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
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
  { delphi }
  Classes,
  { DUnit }
  TestFrameWork,
  { local }
  JcfStringUtils,
  JcfMiscFunctions;

type
  TBaseTestProcess = class(TTestCase)
  private
  published

    procedure  TestEmpty;
    procedure  TestOneLine;
    procedure  TestTwoLines;
    procedure  TestTwoEnded;
    procedure  TestThreeLines;
    procedure  TestSixLines;
    procedure  TestBlankLines;
    procedure  TestBlankLines2;

    procedure TestUnixLineEnds;
    procedure TestMixedLineEnds;
    procedure TestMixedLineEnds2;
    procedure TestMixedEmptyLines;

    procedure TestSplitIntoChangeSectionsEmpty;
    procedure TestSplitIntoChangeSectionsOneLineSame;
    procedure TestSplitIntoChangeSectionsOneLineDiff;

    procedure TestSplitIntoChangeSectionsStartSame;
    procedure TestSplitIntoChangeSectionsEndSame;
    procedure TestSplitIntoChangeSectionsMid;
  end;


implementation

procedure TBaseTestProcess.TestEmpty;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines('');
  try

    CheckEquals(0,lcOut.Count);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestOneLine;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines('fred');
  try

    CheckEquals(1,lcOut.Count);
    CheckEquals('fred', lcOut[0]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestTwoLines;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines('fred' + NativeCrLf + 'Bloggs');
  try

    CheckEquals(2,lcOut.Count);
    CheckEquals('fred' + NativeCrLf, lcOut[0]);
    CheckEquals('Bloggs', lcOut[1]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestTwoEnded;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines('fred' + NativeCrLf + 'Bloggs' + NativeCrLf);
  try

    CheckEquals(2,lcOut.Count);
    CheckEquals('fred' + NativeCrLf, lcOut[0]);
    CheckEquals('Bloggs' + NativeCrLf, lcOut[1]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestThreeLines;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines('fred' + NativeCrLf + 'Bloggs' + NativeCrLf + 'Fish');
  try

    CheckEquals(3,lcOut.Count);
    CheckEquals('fred' + NativeCrLf, lcOut[0]);
    CheckEquals('Bloggs' + NativeCrLf, lcOut[1]);
    CheckEquals('Fish', lcOut[2]);
  finally
    lcOut.Free;
  end;
end;


procedure TBaseTestProcess.TestSixLines;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(
    'fred' + NativeCrLf +
    'Bloggs' + NativeCrLf +
    'Fish' + NativeCrLf +
    'Fish' + NativeCrLf +
    'Fish' + NativeCrLf +
    'Fish');
  try

    CheckEquals(6,lcOut.Count);
    CheckEquals('fred' + NativeCrLf, lcOut[0]);
    CheckEquals('Bloggs' + NativeCrLf, lcOut[1]);
    CheckEquals('Fish' + NativeCrLf, lcOut[2]);
  finally
    lcOut.Free;
  end;

end;

procedure TBaseTestProcess.TestBlankLines;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(NativeCrLf + NativeCrLf);
  try

    CheckEquals(2,lcOut.Count);
    CheckEquals(NativeCrLf, lcOut[0]);
    CheckEquals(NativeCrLf, lcOut[1]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestBlankLines2;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(NativeCrLf + 'foo' + NativeCrLf + NativeCrLf);
  try

    CheckEquals(3,lcOut.Count);
    CheckEquals(NativeCrLf, lcOut[0]);
    CheckEquals('foo' + NativeCrLf, lcOut[1]);
    CheckEquals(NativeCrLf, lcOut[2]);
  finally
    lcOut.Free;
  end;
end;


procedure TBaseTestProcess.TestUnixLineEnds;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(
    'fred' + NativeLineFeed +
    'Bloggs' + NativeLineFeed);
  try

    CheckEquals(2,lcOut.Count);
    CheckEquals('fred' + NativeLineFeed, lcOut[0]);
    CheckEquals('Bloggs' + NativeLineFeed, lcOut[1]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestMixedLineEnds;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(
    'fred' + NativeLineFeed +
    'Bloggs' + NativeCrLf +
    'Fish' +  NativeLineFeed);
  try

    CheckEquals(3,lcOut.Count);
    CheckEquals('fred' + NativeLineFeed, lcOut[0]);
    CheckEquals('Bloggs' + NativeCrLf, lcOut[1]);
    CheckEquals('Fish' + NativeLineFeed, lcOut[2]);
  finally
    lcOut.Free;
  end;
end;


procedure TBaseTestProcess.TestMixedLineEnds2;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(
    'fred' + NativeCrLf +
    'Bloggs' + NativeLineFeed +
    'Fish' +  NativeCrLf);
  try

    CheckEquals(3,lcOut.Count);
    CheckEquals('fred' + NativeCrLf, lcOut[0]);
    CheckEquals('Bloggs' + NativeLineFeed, lcOut[1]);
    CheckEquals('Fish' + NativeCrLf, lcOut[2]);
  finally
    lcOut.Free;
  end;
end;

procedure TBaseTestProcess.TestMixedEmptyLines;
var
  lcOut: TStrings;
begin
  lcOut := SplitIntoLines(NativeCrLf + NativeLineFeed + NativeCrLf);
  try

    CheckEquals(3,lcOut.Count);
    CheckEquals(NativeCrLf, lcOut[0]);
    CheckEquals(NativeLineFeed, lcOut[1]);
    CheckEquals(NativeCrLf, lcOut[2]);
  finally
    lcOut.Free;
  end;
end;


procedure TBaseTestProcess.TestSplitIntoChangeSectionsEmpty;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two empty lists - nothing happens
    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(0, lcStart.Count);
    CheckEquals(0, lcEnd.Count);
  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;

procedure TBaseTestProcess.TestSplitIntoChangeSectionsOneLineSame;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two identical lists - all put in the "same start" section
    lcOne.Add('Foo');
    lcTwo.Add('Foo');

    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(0, lcOne.Count);
    CheckEquals(0, lcTwo.Count);

    CheckEquals(1, lcStart.Count);
    CheckEquals(0, lcEnd.Count);
  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;

procedure TBaseTestProcess.TestSplitIntoChangeSectionsOneLineDiff;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two different lists - no same start or same end
    lcOne.Add('Foo');
    lcTwo.Add('Bar');

    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(1, lcOne.Count);
    CheckEquals(1, lcTwo.Count);

    CheckEquals(0, lcStart.Count);
    CheckEquals(0, lcEnd.Count);
  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;


procedure TBaseTestProcess.TestSplitIntoChangeSectionsStartSame;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two lists that start the same
    lcOne.Add('Same');
    lcOne.Add('Foo');

    lcTwo.Add('Same');
    lcTwo.Add('Bar');

    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(1, lcOne.Count);
    CheckEquals(1, lcTwo.Count);

    CheckEquals(1, lcStart.Count);
    CheckEquals('Same', lcStart[0]);

    CheckEquals(0, lcEnd.Count);
  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;


procedure TBaseTestProcess.TestSplitIntoChangeSectionsEndSame;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two lists that end the same
    lcOne.Add('Foo');
    lcOne.Add('Same');

    lcTwo.Add('Bar');
    lcTwo.Add('Same');

    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(1, lcOne.Count);
    CheckEquals(1, lcTwo.Count);

    CheckEquals(0, lcStart.Count);
    CheckEquals(1, lcEnd.Count);
    CheckEquals('Same', lcEnd[0]);
  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;

procedure TBaseTestProcess.TestSplitIntoChangeSectionsMid;
var
  lcOne, lcTwo, lcStart, lcEnd: TStrings;
begin
  lcOne := TStringList.Create();
  lcTwo := TStringList.Create();
  lcStart := TStringList.Create();
  lcEnd := TStringList.Create();
  try
    // two lists that start and end the same 
    lcOne.Add('Start same');
    lcOne.Add('Foo');
    lcOne.Add('End same');

    lcTwo.Add('Start same');
    lcTwo.Add('Bar');
    lcTwo.Add('End same');

    SplitIntoChangeSections(lcOne, lcTwo, lcStart, lcEnd);
    CheckEquals(1, lcOne.Count);
    CheckEquals(1, lcTwo.Count);

    CheckEquals(1, lcStart.Count);
    CheckEquals('Start same', lcStart[0]);

    CheckEquals(1, lcEnd.Count);
    CheckEquals('End same', lcEnd[0]);

  finally
    lcOne.Free;
    lcTwo.Free;
    lcStart.Free;
    lcEnd.Free;
  end;
end;

initialization
  TestFramework.RegisterTest(TBaseTestProcess.Suite);
end.
