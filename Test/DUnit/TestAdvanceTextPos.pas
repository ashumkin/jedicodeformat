unit TestAdvanceTextPos;

  { test cases for AdvanceTextPos function
    which Adem Baba is trying to speed up

  This regression will ensure that the functionality is unchanged }

interface

uses
  Classes,
  JclStrings,
  TestFrameWork,
  TestConverter, JcfMiscFunctions;

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

uses SysUtils, ABTempFuncs;

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
      AbabaAdvanceTextPos('', lix, liy);

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
        AbabaAdvanceTextPos(lsTest, lix, liy);

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
    lsTest := StrRepeat(AnsiLineBreak, liStringLengthLoop);

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AbabaAdvanceTextPos(lsTest, liX, liY);

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
    lsTest := StrRepeat('X', liStringLengthLoop) + AnsiLineBreak;

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AbabaAdvanceTextPos(lsTest, liX, liY);

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
    lsTest := StrRepeat(lsLine + AnsiLineBreak, liStringLengthLoop) + lsLine;

    for liXLoop := 0 to MAX_LOOP do
    begin
      for liYLoop := 0 to MAX_LOOP do
      begin
        liX := liXLoop;
        liy := liYLoop;
        AbabaAdvanceTextPos(lsTest, liX, liY);

        CheckEquals(liStringLengthLoop, liX,
          'X value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + liStringLengthLoop, liY,
          'Y value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));

        { and if it ends in a return... }
        liX := liXLoop;
        liy := liYLoop;
        lsTest2 := lsTest + 'fooo' + AnsiLinebreak;

        AbabaAdvanceTextPos(lsTest2, liX, liY);

        CheckEquals(1, liX,
          'X2 value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
        CheckEquals(liYLoop + liStringLengthLoop + 1, liY,
          'Y2 value for ' + IntToStr(liXLoop) + ' ' + IntToStr(liYLoop) + ' ' + IntToStr(liStringLengthLoop));
      end;
    end;
  end;
end;

initialization
 TestFramework.RegisterTest('', TTestAdvanceTextPos.Suite);

end.
