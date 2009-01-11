unit TestAlignment;

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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ AFS 31 March 2003
  Test alignment processes }

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
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils,
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
    'const' + NativeLineBreak +
    '  a = 3;' + NativeLineBreak +
    '  bee = 3;' + NativeLineBreak +
    '  deedee = 4.567;' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + NativeLineBreak +
    '  a      = 3;' + NativeLineBreak +
    '  bee    = 3;' + NativeLineBreak +
    '  deedee = 4.567;' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst2;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + NativeLineBreak +
    'const' + NativeLineBreak +
    '  a = 3;' + NativeLineBreak +
    '  bee = 3;' + NativeLineBreak +
    '  deedee = 4.567;' + NativeLineBreak +
    ' begin end; ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + NativeLineBreak +
    'const' + NativeLineBreak +
    '  a      = 3;' + NativeLineBreak +
    '  bee    = 3;' + NativeLineBreak +
    '  deedee = 4.567;' + NativeLineBreak +
    ' begin end; ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst3;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'const' + NativeLineBreak +
    '  a = 3; ' + NativeLineBreak +
    '  bee = 3;  ' + NativeLineBreak +
    '  deedee = 4.567;   ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + NativeLineBreak +
    '  a      = 3; ' + NativeLineBreak +
    '  bee    = 3;  ' + NativeLineBreak +
    '  deedee = 4.567;   ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignConst4;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'const' + NativeLineBreak +
    '  a = 3;' + NativeLineBreak +
    '  bee = 3;' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'const' + NativeLineBreak +
    '  a   = 3;' + NativeLineBreak +
    '  bee = 3;' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignConst, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignVars;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    '  bee: string;' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a:   integer;' + NativeLineBreak +
    '  bee: string;' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignVars, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignVars2;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    '  bee: string;' + NativeLineBreak +
    '  deedee: float;' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a:      integer;' + NativeLineBreak +
    '  bee:    string;' + NativeLineBreak +
    '  deedee: float;' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignVars, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


// input for align vars and allign assign tests
const
  MULTI_ALIGN_IN_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    '  bee: string;' + NativeLineBreak +
    '  deedee: float;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    ' a := 3;' + NativeLineBreak +
    ' bee := ''foo'';' + NativeLineBreak +
    ' deedee := 34.56;' + NativeLineBreak +
    'end;' +
    UNIT_FOOTER;

procedure TTestAlignment.TestAlignVars3;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a:      integer;' + NativeLineBreak +
    '  bee:    string;' + NativeLineBreak +
    '  deedee: float;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    ' a := 3;' + NativeLineBreak +
    ' bee := ''foo'';' + NativeLineBreak +
    ' deedee := 34.56;' + NativeLineBreak +
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
    'procedure foo;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    '  bee: string;' + NativeLineBreak +
    '  deedee: float;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    ' a      := 3;' + NativeLineBreak +
    ' bee    := ''foo'';' + NativeLineBreak +
    ' deedee := 34.56;' + NativeLineBreak +
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
    'procedure foo;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a, aa: integer;' + NativeLineBreak +
    '  bee, bee2: string;' + NativeLineBreak +
    '  deedee, deedee2: float;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    ' a := 3;' + NativeLineBreak +
    ' bee := ''foo'';' + NativeLineBreak +
    ' deedee := 34.56;' + NativeLineBreak +
    ' Foo;' + NativeLineBreak +
    ' Bar;' + NativeLineBreak +
    ' aa := 3;' + NativeLineBreak +
    ' deedee2 := 34.56;' + NativeLineBreak +
    ' bee2 := ''foo'';' + NativeLineBreak +
    'end;' +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    'procedure foo;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a, aa: integer;' + NativeLineBreak +
    '  bee, bee2: string;' + NativeLineBreak +
    '  deedee, deedee2: float;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    ' a      := 3;' + NativeLineBreak +
    ' bee    := ''foo'';' + NativeLineBreak +
    ' deedee := 34.56;' + NativeLineBreak +
    ' Foo;' + NativeLineBreak +
    ' Bar;' + NativeLineBreak +
    ' aa      := 3;' + NativeLineBreak +
    ' deedee2 := 34.56;' + NativeLineBreak +
    ' bee2    := ''foo'';' + NativeLineBreak +
    'end;' +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignAssign, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;



procedure TTestAlignment.TestAlignComments;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' // foo' + NativeLineBreak +
    '     { bar bie } ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    '     // foo' + NativeLineBreak +
    '     { bar bie } ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignComments2;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' // foo' + NativeLineBreak +
    '    { bar bie } ' + NativeLineBreak +
    '      // baz' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    '      // foo' + NativeLineBreak +
    '      { bar bie } ' + NativeLineBreak +
    '      // baz' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignTypedef;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo = integer; ' + NativeLineBreak +
    '  barnee = string; ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo    = integer; ' + NativeLineBreak +
    '  barnee = string; ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignTypedef2;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo = integer; ' + NativeLineBreak +
    '  barnee = string; ' + NativeLineBreak +
    '  Baaaaaaz = float; ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo      = integer; ' + NativeLineBreak +
    '  barnee   = string; ' + NativeLineBreak +
    '  Baaaaaaz = float; ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignTypedef, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignFields;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo = class ' + NativeLineBreak +
    '  private' + NativeLineBreak +
    '   aaaa: integer;' + NativeLineBreak +
    '   aaaaaa: integer;' + NativeLineBreak +
    '   aa: integer;' + NativeLineBreak +
    '  public' + NativeLineBreak +
    '   fi: integer;' + NativeLineBreak +
    '   fi1: integer;' + NativeLineBreak +
    '   fi111: integer;' + NativeLineBreak +
    '  end; ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  foo = class ' + NativeLineBreak +
    '  private' + NativeLineBreak +
    '   aaaa:   integer;' + NativeLineBreak +
    '   aaaaaa: integer;' + NativeLineBreak +
    '   aa:     integer;' + NativeLineBreak +
    '  public' + NativeLineBreak +
    '   fi:    integer;' + NativeLineBreak +
    '   fi1:   integer;' + NativeLineBreak +
    '   fi111: integer;' + NativeLineBreak +
    '  end; ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignField, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestAlignment.TestAlignFields2;
const
  IN_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  TRfoo = record ' + NativeLineBreak +
    '   fi: integer;' + NativeLineBreak +
    '   fi1: integer;' + NativeLineBreak +
    '   fi111: integer;' + NativeLineBreak +
    '  end; ' + NativeLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER + NativeLineBreak +
    ' type ' + NativeLineBreak +
    '  TRfoo = record ' + NativeLineBreak +
    '   fi:    integer;' + NativeLineBreak +
    '   fi1:   integer;' + NativeLineBreak +
    '   fi111: integer;' + NativeLineBreak +
    '  end; ' + NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TAlignField, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAlignment.Suite);
end.
