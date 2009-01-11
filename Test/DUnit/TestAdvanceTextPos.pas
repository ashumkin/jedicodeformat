unit TestAdvanceTextPos;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAdvanceTextPos
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

  { test cases for AdvanceTextPos function
    which Adem Baba is trying to speed up

  This regression will ensure that the functionality is unchanged }

uses
  { DUnit }
  TestFrameWork,
  { Local }
  JcfStringUtils,
  JcfMiscFunctions;

type
  TTestAdvanceTextPos = class(TTestCase)

  published
    procedure TestNull;
    procedure TestShortString;
    procedure TestReturns;
    procedure TestReturnsAtEnd;
    procedure TestMultipleReturns;
  end;

implementation

uses SysUtils;

{ 1 - idiot test.
  Adding an empty string should not advance the text pos }
procedure TTestAdvanceTextPos.TestNull;
const
  MAX_LOOP = 20;
var
  liXLoop, liYLoop: Integer;
  lix, liy: Integer;
begin
  for liXLoop := 0 to MAX_LOOP do
  begin
    for liYLoop := 0 to MAX_LOOP do
    begin
      liX := liXLoop;
      liy := liYLoop;
      AdvanceTextPos('', lix, liy);

      CheckEquals(liXLoop, liX);
      CheckEquals(liYLoop, liY);
    end;
  end;
end;

{ 2 - simple test. Adding a string of (x) chars long should increase the X pos by (x)
  and leave the Y pos unchanged }
procedure TTestAdvanceTextPos.TestShortString;
const
  MAX_LOOP = 20;
var
  liStringLengthLoop: Integer;
  liXLoop, liYLoop: Integer;
  lix, liy: Integer;
  lsTest: string;
begin
  for liStringLengthLoop := 1 to 5 do
  begin
    lsTest := StrRepeat('X', liStringLengthLoop);

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AdvanceTextPos(lsTest, lix, liy);

        CheckEquals(liXLoop + liStringLengthLoop, liX,
          'X value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop, liY,
          'Y value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
      end;
    end;
  end;
end;

{ 3 - test returns. Adding a return increments the Y pos and resets the X pos }
procedure TTestAdvanceTextPos.TestReturns;
const
  MAX_LOOP = 10;
var
  liStringLengthLoop: Integer;
  liXLoop, liYLoop: Integer;
  lix, liy: Integer;
  lsTest: string;
begin
  for liStringLengthLoop := 1 to 20 do
  begin
    { add one or more returns }
    lsTest := StrRepeat(NativeLineBreak, liStringLengthLoop);

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AdvanceTextPos(lsTest, liX, liY);

        CheckEquals(1, liX,
          'X value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + liStringLengthLoop, liY,
          'Y value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
      end;
    end;
  end;
end;


{ 4 - returns test. Adding on a string that ends in a return
 should increment the y pos and set the x pos to 1
 It doesn't matter what comes before the return }
procedure TTestAdvanceTextPos.TestReturnsAtEnd;
const
  MAX_LOOP = 10;
var
  liStringLengthLoop: Integer;
  liXLoop, liYLoop: Integer;
  lix, liy: Integer;
  lsTest: string;
begin
  for liStringLengthLoop := 1 to 10 do
  begin
    lsTest := StrRepeat('X', liStringLengthLoop) + NativeLineBreak;

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AdvanceTextPos(lsTest, liX, liY);

        CheckEquals(1, liX,
          'X value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + 1, liY,
          'Y value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
      end;
    end;
  end;
end;


{ 5 - complex returns test with a string that contains returns
  and has chars after the last return 

  Adding on a string that contains returns
  Should increment the Y pos by the number of returns in the string
  and set the X pos to the length of the text after the final return }
procedure TTestAdvanceTextPos.TestMultipleReturns;
const
  MAX_LOOP = 10;
var
  liStringLengthLoop: Integer;
  liXLoop, liYLoop: Integer;
  lix, liy: Integer;
  lsLine, lsTest, lsTest2: string;
begin
  for liStringLengthLoop := 1 to 10 do
  begin
    { as liStringLengthLoop, the lines get longer, and there's more of them }
    lsLine := StrRepeat('X', liStringLengthLoop);
    lsTest := StrRepeat(lsLine + NativeLineBreak, liStringLengthLoop) + lsLine;

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AdvanceTextPos(lsTest, liX, liY);

        CheckEquals(liStringLengthLoop, liX,
          'X value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + liStringLengthLoop, liY,
          'Y value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));

        { and if it ends in a return... }
        liX := liXLoop;
        liy := liYLoop;
        lsTest2 := lsTest + 'fooo' + NativeLineBreak;

        AdvanceTextPos(lsTest2, liX, liY);

        CheckEquals(1, liX,
          'X2 value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + liStringLengthLoop + 1, liY,
          'Y2 value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
      end;
    end;
  end;
end;

initialization
 TestFramework.RegisterTest('Procs', TTestAdvanceTextPos.Suite);

end.
