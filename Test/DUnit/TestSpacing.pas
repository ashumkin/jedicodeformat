unit TestSpacing;

{ AFS Jan 2003
  Test spacing processes }

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess;

type
  TTestSpacing = class(TBaseTestProcess)
  private

  protected
  published

    procedure TestNoReturnAfter;
    procedure TestNoReturnBefore;
    procedure TestNoSpaceAfter;
    procedure TestNoSpaceBefore;

    procedure TestSingleSpaceBefore;
    procedure TestSingleSpaceAfter;

    procedure TestReturnBefore;
    procedure TestReturnAfter;
  end;

implementation

uses JclStrings,
  NoReturnAfter, NoReturnBefore, NoSpaceAfter, NoSpaceBefore,
  SingleSpaceBefore, SingleSpaceAfter,
  ReturnBefore, ReturnAfter;

procedure TTestSpacing.TestNoReturnAfter;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin if ' + AnsiLineBreak + '(foo) then ; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin if (foo) then ; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoReturnAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoReturnBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a ' + AnsiLineBreak + ':= 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoReturnBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoSpaceAfter;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := ( 2); end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := (2); end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoSpaceBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo ; begin a := 2 ; end ; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoSpaceBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSpacing.TestSingleSpaceBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER +  ' procedure foo; begin a    := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestSingleSpaceAfter;
const
  IN_UNIT_TEXT = UNIT_HEADER +  ' procedure foo; begin a :=2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestReturnBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER +  ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = 'unit Test;'#$D#$A' interface'#$D#$A' implementation  procedure foo; begin a := 2; end;'#$D#$A'  end.';
begin
  TestProcessResult(TReturnBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestReturnAfter;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = 'unit Test;'#$D#$A#$D#$A' interface'#$D#$A#$D#$A' implementation'#$D#$A#$D#$A'  procedure foo;'#$D#$A' begin'#$D#$A' a := 2;'#$D#$A' end;'#$D#$A'  end.';
begin
  TestProcessResult(TReturnAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);end;


initialization
 TestFramework.RegisterTest(TTestSpacing.Suite);
end.
