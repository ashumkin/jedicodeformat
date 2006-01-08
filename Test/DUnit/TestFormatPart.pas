unit TestFormatPart;

interface

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes;

type
  TTestFormatPart = class(TBaseTestProcess)
  private

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

uses JclStrings, BlockStyles, JcfSettings, SetReturns;

{ TTestFormatPart }

const
  UNIT_IN = UNIT_HEADER +
    'procedure fred; ' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'end;'  + AnsiLineBreak +
    UNIT_FOOTER;

  UNIT_OUT_TOP = SPACED_UNIT_HEADER +
    'procedure fred; ' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'end;'  + AnsiLineBreak +
    UNIT_FOOTER;

  UNIT_OUT_TOP_MID = SPACED_UNIT_HEADER +  AnsiLineBreak +
    'procedure fred; ' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'end;'  + AnsiLineBreak +
    UNIT_FOOTER;


  UNIT_OUT_MID = UNIT_HEADER + 
    'procedure fred; ' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'end;'  + AnsiLineBreak +
    UNIT_FOOTER;

procedure TTestFormatPart.Setup;
begin
  inherited;

end;

procedure TTestFormatPart.Teardown;
begin
  inherited;

end;

procedure TTestFormatPart.Test1;
begin
  TestFormatPartResult(UNIT_IN, UNIT_OUT_TOP, 10, 20);
end;

procedure TTestFormatPart.Test2;
begin
  TestFormatPartResult(UNIT_IN, UNIT_OUT_TOP_MID, 20, 30);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestFormatPart.Suite);
end.
