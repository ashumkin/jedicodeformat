unit TestSpacing;

{ AFS Jan 2003
  Test spacing processes }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestSpacing, released May 2003.
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
    procedure TestNoSpaceBefore2;
    procedure TestNoSpaceBeforeColon;

    procedure TestNoSpaceAfterOperator;
    procedure TestNoSpaceAfterOperator2;

    procedure TestSingleSpaceBefore;

    procedure TestSingleSpaceAfter;
    procedure TestSingleSpaceAfter2;
    procedure TestSingleSpaceAfter3;

    procedure TestSingleSpaceAfterColon;
    procedure TestSingleSpaceAfterColon2;

    procedure TestReturnBefore;
    procedure TestReturnAfter;

    procedure TestBlankLinesAfterProcHeader;

    procedure TestBlankLinesAfterBegin;
    procedure TestBlankLinesBeforeEnd;

    procedure TestUsesCaps1;
    procedure TestUsesCaps2;
    procedure TestUsesCaps3;
  end;

implementation

uses JclStrings,
  JcfSettings,
  NoReturnAfter, NoReturnBefore, NoSpaceAfter, NoSpaceBefore,
  SpaceBeforeColon,
  SingleSpaceBefore, SingleSpaceAfter,
  ReturnBefore, ReturnAfter, RemoveBlankLinesAfterProcHeader,
  RemoveReturnsAfterBegin, RemoveReturnsBeforeEnd, UnitNameCaps;

procedure TTestSpacing.TestNoReturnAfter;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin if ' + AnsiLineBreak + '(foo) then ; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin if (foo) then ; end; ' + UNIT_FOOTER;
begin
  FormatSettings.Returns.RemoveBadReturns := True;
  TestProcessResult(TNoReturnAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoReturnBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a ' + AnsiLineBreak + ':= 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  FormatSettings.Returns.RemoveBadReturns := True;
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

procedure TTestSpacing.TestNoSpaceBefore2;
const
  IN_UNIT_TEXT = INTERFACE_HEADER + 'function foo (i : integer) : integer ; far ; stdcall ;' +
    ' implementation ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'function foo(i: integer): integer; far; stdcall;' +
    ' implementation ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoSpaceBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

{ preserve line breaks if the user put them in }

procedure TTestSpacing.TestNoSpaceBeforeColon;
const
  //JcfSettings.SetSpaces.SpacesBeforeColonFn := 0;
  IN_UNIT_TEXT = UNIT_HEADER + ' function foo : integer; begin result := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoSpaceAfterOperator;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' function foo: ' + AnsiLineBreak +
    '  integer; begin result := 2 + 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: '  + AnsiLineBreak +
    '  integer; begin result := 2 + 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestNoSpaceAfterOperator2;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' function foo: ' + AnsiLineBreak +
    '  integer; begin result := 2 * - 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: '  + AnsiLineBreak +
    '  integer; begin result := 2 * -2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TNoSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
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
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a :=2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestSingleSpaceAfter2;
const
  IN_UNIT_TEXT = INTERFACE_HEADER + 'function foo(i:integer):integer;far;stdcall;' +
    ' implementation ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'function foo(i: integer): integer; far; stdcall;' +
    ' implementation ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestSingleSPaceAfter3;
const
  IN_UNIT_TEXT = INTERFACE_HEADER + 'type TFredProc =procedure(var psFred:integer)of Object;' +
    ' implementation ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = INTERFACE_HEADER + 'type TFredProc = procedure(var psFred: integer)of Object;' +
    ' implementation ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSpacing.TestSingleSpaceAfterColon;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' function foo:   integer; begin result := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSpacing.TestSingleSpaceAfterColon2;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' function foo: ' + AnsiLineBreak +
    '  integer; begin result := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: '  + AnsiLineBreak +
    '  integer; begin result := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TSingleSpaceAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSpacing.TestReturnBefore;
const
  IN_UNIT_TEXT = UNIT_HEADER +  'procedure foo; begin a := 2; end;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = 'unit Test;' + AnsiLineBreak + AnsiLineBreak + 'interface' +
    AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo; ' + AnsiLineBreak + 'begin a := 2; ' + AnsiLineBreak +
    'end;' + AnsiLineBreak + AnsiLineBreak + 'end.';
begin
  TestProcessResult(TReturnBefore, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestReturnAfter;
const
  UNLINED_UNIT_HEADER = 'unit Test; interface implementation';
  IN_UNIT_TEXT = UNLINED_UNIT_HEADER + ' procedure foo; begin a := 2; end;' + UNIT_FOOTER;
  OUT_UNIT_TEXT = 'unit Test;' + AnsiLineBreak + AnsiLineBreak +
    ' interface' + AnsiLineBreak + AnsiLineBreak +
    ' implementation' + AnsiLineBreak + AnsiLineBreak +
    ' procedure foo;' + AnsiLineBreak + ' begin' + AnsiLineBreak +
    ' a := 2;' + AnsiLineBreak + ' end;' + AnsiLineBreak + AnsiLineBreak + 'end.';
begin
  TestProcessResult(TReturnAfter, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSpacing.TestBlankLinesAfterProcHeader;
const
  IN_UNIT_TEXT = UNIT_HEADER + 'procedure foo;' + AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak + 'begin a := 2; end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + 'procedure foo;' + AnsiLineBreak + AnsiLineBreak +
    'begin a := 2; end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestBlankLinesAfterBegin;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin' + AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak + 'end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin' + AnsiLineBreak + AnsiLineBreak +
    'end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveReturnsAfterBegin, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpacing.TestBlankLinesBeforeEnd;
const
  IN_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin' + AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak + 'end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure foo; begin' + AnsiLineBreak + AnsiLineBreak +
    'end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveReturnsBeforeEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;



procedure TTestSpacing.TestUsesCaps1;
const
  IN_UNIT_TEXT = 'unit foo; interface uses bar; implementation uses fish; end.';
begin
  FormatSettings.UnitNameCaps.Enabled := True;
  TestProcessResult(TUnitNameCaps, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSpacing.TestUsesCaps2;
const
  IN_UNIT_TEXT  = 'unit foo; interface uses bar; implementation uses fish, spon; end.';
  OUT_UNIT_TEXT = 'unit Foo; interface uses Bar; implementation uses Fish, spon; end.';
begin
  // contains these and only these
  FormatSettings.UnitNameCaps.Enabled := True;
  FormatSettings.UnitNameCaps.Clear;
  FormatSettings.UnitNameCaps.Add('Foo');
  FormatSettings.UnitNameCaps.Add('Bar');
  FormatSettings.UnitNameCaps.Add('Fish');

  TestProcessResult(TUnitNameCaps, IN_UNIT_TEXT, OUT_UNIT_TEXT);

  // reset
  FormatSettings.Read;
end;

procedure TTestSpacing.TestUsesCaps3;
const
  IN_UNIT_TEXT  = 'unit foo; interface implementation uses fish; ' +
    'initialization monkey.soy; shatner.kirk := shatner.kirk + 3; end.';
  OUT_UNIT_TEXT = 'unit Foo; interface implementation uses Fish; ' +
    'initialization Monkey.soy; shatner.kirk := shatner.kirk + 3; end.';
begin
  // contains these and only these
  FormatSettings.UnitNameCaps.Enabled := True;
  FormatSettings.UnitNameCaps.Clear;
  FormatSettings.UnitNameCaps.Add('Foo');
  FormatSettings.UnitNameCaps.Add('Bar');
  FormatSettings.UnitNameCaps.Add('Fish');
  FormatSettings.UnitNameCaps.Add('Monkey');
  // this won't be used as 'soy' is a fn not a unit name
  FormatSettings.UnitNameCaps.Add('Soy');
  // likewise kirk is a global var
  FormatSettings.UnitNameCaps.Add('Kirk');

  TestProcessResult(TUnitNameCaps, IN_UNIT_TEXT, OUT_UNIT_TEXT);

  // reset
  FormatSettings.Read;
end;

initialization
 TestFramework.RegisterTest(TTestSpacing.Suite);
end.