unit TestAlignment;

{ AFS 31 March 2003
  Test alignment processes }

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess;

type
  TTestAlignment = class(TBaseTestProcess)
  private
  public
    procedure Setup; override;
  published
    procedure TestAlignConst;
    procedure TestAlignConst2;
    procedure TestAlignConst3;
    procedure TestAlignConst4;

    procedure TestAlignVars;
    procedure TestAlignVars2;
    procedure TestAlignVars3;

    procedure TestAlignAssign;
    procedure TestAlignAssign2;

    procedure TestAlignComments;
    procedure TestAlignComments2;

    procedure TestAlignTypedef;
    procedure TestAlignTypedef2;

  end;

implementation

uses
  JclStrings,
  JcfSettings, AlignConst, AlignVars, AlignAssign, AlignComment,
  AlignTypedef;

procedure TTestAlignment.Setup;
begin
  inherited;
  FormatSettings.Align.MaxVariance := 5;
end;

procedure TTestAlignment.TestAlignConst;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a = 3;' + AnsiLineBreak +
    '  bee = 3;' + AnsiLineBreak +
    '  deedee = 4.567;' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a      = 3;' + AnsiLineBreak +
    '  bee    = 3;' + AnsiLineBreak +
    '  deedee = 4.567;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst2;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'const' + AnsiLineBreak +
    '  a = 3;' + AnsiLineBreak +
    '  bee = 3;' + AnsiLineBreak +
    '  deedee = 4.567;' + AnsiLineBreak +
    ' begin end; ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'const' + AnsiLineBreak +
    '  a      = 3;' + AnsiLineBreak +
    '  bee    = 3;' + AnsiLineBreak +
    '  deedee = 4.567;' + AnsiLineBreak +
    ' begin end; ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst3;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a = 3; ' + AnsiLineBreak +
    '  bee = 3;  ' + AnsiLineBreak +
    '  deedee = 4.567;   ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a      = 3; ' + AnsiLineBreak +
    '  bee    = 3;  ' + AnsiLineBreak +
    '  deedee = 4.567;   ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst4;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a = 3;' + AnsiLineBreak +
    '  bee = 3;' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + AnsiLineBreak +
    '  a   = 3;' + AnsiLineBreak +
    '  bee = 3;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignVars;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'var' + AnsiLineBreak +
    '  a: integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'var' + AnsiLineBreak +
    '  a:   integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignVars, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignVars2;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'var' + AnsiLineBreak +
    '  a: integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'var' + AnsiLineBreak +
    '  a:      integer;' + AnsiLineBreak +
    '  bee:    string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignVars, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


// input for align vars and allign assign tests
const
  MULTI_ALIGN_IN_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a: integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    'begin' +  AnsiLineBreak +
    ' a := 3;' + AnsiLineBreak +
    ' bee := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;

procedure TTestAlignment.TestAlignVars3;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a:      integer;' + AnsiLineBreak +
    '  bee:    string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    ' a := 3;' + AnsiLineBreak +
    ' bee := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;

begin
  TestProcessResult(TAlignVars, MULTI_ALIGN_IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestAlignment.TestAlignAssign;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a: integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    'begin' +  AnsiLineBreak +
    ' a      := 3;' + AnsiLineBreak +
    ' bee    := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignAssign, MULTI_ALIGN_IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

{ this one tests that
 - multiple blocks work
 - they align independantly
 - don't necessarily align on last line }
procedure TTestAlignment.TestAlignAssign2;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a, aa: integer;' + AnsiLineBreak +
    '  bee, bee2: string;' + AnsiLineBreak +
    '  deedee, deedee2: float;' + AnsiLineBreak +
    'begin' +  AnsiLineBreak +
    ' a := 3;' + AnsiLineBreak +
    ' bee := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    ' Foo;' + AnsiLineBreak +
    ' Bar;' + AnsiLineBreak +
    ' aa := 3;' + AnsiLineBreak +
    ' deedee2 := 34.56;' + AnsiLineBreak +
    ' bee2 := ''foo'';' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a, aa: integer;' + AnsiLineBreak +
    '  bee, bee2: string;' + AnsiLineBreak +
    '  deedee, deedee2: float;' + AnsiLineBreak +
    'begin' +  AnsiLineBreak +
    ' a      := 3;' + AnsiLineBreak +
    ' bee    := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    ' Foo;' + AnsiLineBreak +
    ' Bar;' + AnsiLineBreak +
    ' aa      := 3;' + AnsiLineBreak +
    ' deedee2 := 34.56;' + AnsiLineBreak +
    ' bee2    := ''foo'';' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignAssign, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;



procedure TTestAlignment.TestAlignComments;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' // foo' + AnsiLineBreak +
    '     { bar bie } ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    '     // foo' + AnsiLineBreak +
    '     { bar bie } ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignComments2;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' // foo' + AnsiLineBreak +
    '    { bar bie } ' + AnsiLineBreak +
    '      // baz' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    '      // foo' + AnsiLineBreak +
    '      { bar bie } ' + AnsiLineBreak +
    '      // baz' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignTypedef;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo = integer; ' + AnsiLineBreak +
    '  barnee = string; ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo    = integer; ' + AnsiLineBreak +
    '  barnee = string; ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TALignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignTypedef2;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo = integer; ' + AnsiLineBreak +
    '  barnee = string; ' + AnsiLineBreak +
    '  Baaaaaaz = float; ' +  AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo      = integer; ' + AnsiLineBreak +
    '  barnee   = string; ' + AnsiLineBreak +
    '  Baaaaaaz = float; ' +  AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TALignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest(TTestAlignment.Suite);
end.
