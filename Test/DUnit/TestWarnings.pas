unit TestWarnings;

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter;

type
  TTestWarnings = class(TTestCase)
  private
    fcConvert: TStringsConverter;
    fcInput, fcOutput, fcMessages: TStringList;

    procedure TestNoWarnings(const psUnit: string);
    procedure TestWarnings(const psUnit: string; const psWarningMatch: string); overload;
    procedure TestWarnings(const psUnit: string; const psWarningMatches: array of string); overload;
    procedure TestWarnings(const psUnit: string; const piMatchCount: integer; const psWarningMatches: array of string); overload;

  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    // no warnings in basic units
    procedure TestNoWarningsBasic;

    // warnings on empty stuff
    procedure TestEmptyProcedure;
    procedure TestEmptyBlock;
    procedure TestEmptyTryExcept;
    procedure TestEmptyTryFinally;

    // assign to fn name
    procedure TestAssignToFunctionName;

    // real and real84 types
    procedure TestRealType1;
    procedure TestRealType2;
    procedure TestRealType3;
    procedure TestRealType4;

    // calls to destroy
    procedure TestDestroy;

    // case without else block
    procedure TestCaseNoElse1;

 end;


implementation

uses SysUtils, JclStrings;

const
  UNIT_HEADER = 'unit Test; interface implementation ';
  UNIT_FOOTER = ' end. ';

  EMPTY_BEGIN_END = 'Empty begin..end block';
  EMPTY_TRY = 'Empty try block';
  EMPTY_EXCEPT_END = 'Empty except..end';
  EMPTY_FINALLY_END  = 'Empty finally..end';
  REAL_TYPE_USED = 'Real type used';
  REAL48_TYPE_USED = 'Real48 type used';

procedure TTestWarnings.Setup;
begin
  fcConvert := TStringsConverter.Create;

  fcInput := TStringList.Create;
  fcOutput := TStringList.Create;
  fcMessages := TStringList.Create;

  fcConvert.InputStrings := fcInput;
  fcConvert.OutputStrings := fcOutput;
  fcConvert.MessageStrings := fcMessages;
end;

procedure TTestWarnings.TearDown;
begin
  FreeAndNil(fcConvert);

  FreeAndNil(fcInput);
  FreeAndNil(fcOutput);
  FreeAndNil(fcMessages);
end;

procedure TTestWarnings.TestWarnings(const psUnit, psWarningMatch: string);
begin
  TestWarnings(psUnit, 1, [psWarningMatch]);
end;

procedure TTestWarnings.TestWarnings(const psUnit: string;
  const psWarningMatches: array of string);
var
  liCount: integer;
begin
  liCount := High(psWarningMatches) - Low(psWarningMatches) + 1;
  TestWarnings(psUnit, liCount, psWarningMatches);
end;

procedure TTestWarnings.TestWarnings(const psUnit: string;
  const piMatchCount: integer; const psWarningMatches: array of string);
var
  lbFound: boolean;
  liLoop: integer;
begin
  fcInput.Text := psUnit;
  
  fcConvert.Convert;

  // convert should work
  CheckEquals(False, fcConvert.ConvertError, fcConvert.ConvertErrorMessage);
  // with messages
  CheckEquals(piMatchCount, fcMessages.Count, 'Wrong number of messages');

  for liLoop := Low(psWarningMatches) to High(psWarningMatches) do
  begin
    // containing certain text
    lbFound := (StrIPos(psWarningMatches[liLoop], fcMessages.Text) > 0);

    Check(lbFound, psWarningMatches[liLoop] + ' was not found in output ' + fcMessages.Text);
  end;

  fcConvert.Clear;
end;

procedure TTestWarnings.TestNoWarnings(const psUnit: string);
begin
  TestWarnings(psUnit, 0, []);
end;


procedure TTestWarnings.TestNoWarningsBasic;
const
  UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
begin
  TestNoWarnings(UNIT_TEXT);
end;


procedure TTestWarnings.TestEmptyProcedure;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; begin end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, EMPTY_BEGIN_END);
end;

procedure TTestWarnings.TestEmptyBlock;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; begin begin end; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, EMPTY_BEGIN_END);
end;

procedure TTestWarnings.TestEmptyTryExcept;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; begin try except end; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, [EMPTY_TRY, EMPTY_EXCEPT_END]);
end;

procedure TTestWarnings.TestEmptyTryFinally;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; begin try finally end; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, [EMPTY_TRY, EMPTY_FINALLY_END]);
end;

procedure TTestWarnings.TestAssignToFunctionName;
const
  UNIT_TEXT = UNIT_HEADER + ' function fred: integer; begin fred := 3; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, 'Assignment to the function name');
end;


procedure TTestWarnings.TestRealType1;
const
  UNIT_TEXT = UNIT_HEADER + ' var foo: real; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, REAL_TYPE_USED);
end;

procedure TTestWarnings.TestRealType2;
const
  UNIT_TEXT = UNIT_HEADER + ' const foo: Real48 = 4.5; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, REAL48_TYPE_USED);
end;

procedure TTestWarnings.TestRealType3;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; var foo: Real48; begin end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, [EMPTY_BEGIN_END, REAL48_TYPE_USED]);
end;

procedure TTestWarnings.TestRealType4;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; var foo: Real48; bar: real; begin end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, [EMPTY_BEGIN_END, REAL_TYPE_USED, REAL48_TYPE_USED]);
end;

procedure TTestWarnings.TestDestroy;
const
  UNIT_TEXT = UNIT_HEADER + ' procedure fred; begin Destroy; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT, 'Destroy should not normally be called');
end;

procedure TTestWarnings.TestCaseNoElse1;
const
  UNIT_TEXT = UNIT_HEADER +  'procedure fred; var li: integer; begin case li of 1: end; end; ' + UNIT_FOOTER;
begin
  TestWarnings(UNIT_TEXT,'Case statement has no else case');
end;

initialization
 TestFramework.RegisterTest(TTestWarnings.Suite);
end.
