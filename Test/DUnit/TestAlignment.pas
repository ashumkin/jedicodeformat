unit TestAlignment;

{ AFS 31 March 2003
  Test alignment processes }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestAlignment, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

interface

uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestAlignment = class(TBaseTestProcess)
  private
  public
    procedure SetUp; override;
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

    procedure TestAlignFields;
    procedure TestAlignFields2;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, AlignConst, AlignVars, AlignAssign, AlignComment,
  AlignTypedef, AlignField;

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
    'begin' + AnsiLineBreak +
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
  FormatSettings.Align.AlignAssign := True;
  TestProcessResult(TAlignVars, MULTI_ALIGN_IN_UNIT_TEXT, OUT_UNIT_TEXT);
  TestProcessResult(TAlignVars, OUT_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestAlignment.TestAlignAssign;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'var' + AnsiLineBreak +
    '  a: integer;' + AnsiLineBreak +
    '  bee: string;' + AnsiLineBreak +
    '  deedee: float;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    ' a      := 3;' + AnsiLineBreak +
    ' bee    := ''foo'';' + AnsiLineBreak +
    ' deedee := 34.56;' + AnsiLineBreak +
    'end;' +
    UNIT_FOOTER;
begin
  FormatSettings.Align.AlignAssign := True;
  TestProcessResult(TAlignAssign, MULTI_ALIGN_IN_UNIT_TEXT, OUT_UNIT_TEXT);
  TestProcessResult(TAlignAssign, OUT_UNIT_TEXT, OUT_UNIT_TEXT);
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
    'begin' + AnsiLineBreak +
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
    'begin' + AnsiLineBreak +
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
  TestProcessResult(TAlignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignTypedef2;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo = integer; ' + AnsiLineBreak +
    '  barnee = string; ' + AnsiLineBreak +
    '  Baaaaaaz = float; ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo      = integer; ' + AnsiLineBreak +
    '  barnee   = string; ' + AnsiLineBreak +
    '  Baaaaaaz = float; ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignFields;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo = class ' + AnsiLineBreak +
    '  private' + AnsiLineBreak +
    '   aaaa: integer;' + AnsiLineBreak +
    '   aaaaaa: integer;' + AnsiLineBreak +
    '   aa: integer;' + AnsiLineBreak +
    '  public' + AnsiLineBreak +
    '   fi: integer;' + AnsiLineBreak +
    '   fi1: integer;' + AnsiLineBreak +
    '   fi111: integer;' + AnsiLineBreak +
    '  end; ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  foo = class ' + AnsiLineBreak +
    '  private' + AnsiLineBreak +
    '   aaaa:   integer;' + AnsiLineBreak +
    '   aaaaaa: integer;' + AnsiLineBreak +
    '   aa:     integer;' + AnsiLineBreak +
    '  public' + AnsiLineBreak +
    '   fi:    integer;' + AnsiLineBreak +
    '   fi1:   integer;' + AnsiLineBreak +
    '   fi111: integer;' + AnsiLineBreak +
    '  end; ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignField, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignFields2;
const
  IN_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  TRfoo = record ' + AnsiLineBreak +
    '   fi: integer;' + AnsiLineBreak +
    '   fi1: integer;' + AnsiLineBreak +
    '   fi111: integer;' + AnsiLineBreak +
    '  end; ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + AnsiLineBreak +
    ' type ' + AnsiLineBreak +
    '  TRfoo = record ' + AnsiLineBreak +
    '   fi:    integer;' + AnsiLineBreak +
    '   fi1:   integer;' + AnsiLineBreak +
    '   fi111: integer;' + AnsiLineBreak +
    '  end; ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignField, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAlignment.Suite);
end.
