unit TestSpaceBeforeBrackets;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestSpaceBeforeBrackets, released April 2007.
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

{$I jcfGlobal.inc}

interface

uses
  TestFrameWork, BaseTestProcess;

type
  TTestSpaceBeforeBrackets = class(TBaseTestProcess)
  private
    fbSaveSpaceBeforeOpenBracketsInFunctionDeclaration: boolean;
    fbSaveSpaceBeforeOpenBracketsInFunctionCall: boolean;
    fbSaveSpaceBeforeOpenSquareBracketsInExpression: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestSpaceBeforeBrackets_InBoth;
    procedure TestSpaceBeforeBrackets_InCall;
    procedure TestSpaceBeforeBrackets_InDecl;
    procedure TestSpaceBeforeBrackets_Off;

    procedure TestSpaceBeforeArray_Off;
    procedure TestSpaceBeforeArray_On;
  end;

implementation

uses JclStrings, JclAnsiStrings,
  JcfSettings, SetSpaces, SingleSpaceBefore;

const
  TEST_UNIT_IN = UNIT_HEADER +
  'function foo(value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + foo(1 + 1);' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;

  TEST_UNIT_OUT_BOTH = UNIT_HEADER +
  'function foo (value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + foo (1 + 1);' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;

  TEST_UNIT_OUT_HEADER = UNIT_HEADER +
  'function foo (value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + foo(1 + 1);' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;

  TEST_UNIT_OUT_CALL = UNIT_HEADER +
  'function foo(value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + foo (1 + 1);' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;


procedure TTestSpaceBeforeBrackets.SetUp;
begin
  inherited;

  fbSaveSpaceBeforeOpenBracketsInFunctionDeclaration := FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration;
  fbSaveSpaceBeforeOpenBracketsInFunctionCall := FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall;
  fbSaveSpaceBeforeOpenSquareBracketsInExpression := FormatSettings.Spaces.SpaceBeforeOpenSquareBracketsInExpression;
end;

procedure TTestSpaceBeforeBrackets.TearDown;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := fbSaveSpaceBeforeOpenBracketsInFunctionDeclaration;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := fbSaveSpaceBeforeOpenBracketsInFunctionCall;
  FormatSettings.Spaces.SpaceBeforeOpenSquareBracketsInExpression := fbSaveSpaceBeforeOpenSquareBracketsInExpression;
end;

procedure TTestSpaceBeforeBrackets.TestSpaceBeforeBrackets_Off;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := False;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := False;
  TestProcessResult(TSingleSpaceBefore, TEST_UNIT_IN, TEST_UNIT_IN);
end;


procedure TTestSpaceBeforeBrackets.TestSpaceBeforeBrackets_InDecl;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := True;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := False;
  TestProcessResult(TSingleSpaceBefore, TEST_UNIT_IN, TEST_UNIT_OUT_HEADER);
end;

procedure TTestSpaceBeforeBrackets.TestSpaceBeforeBrackets_InCall;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := False;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := True;
  TestProcessResult(TSingleSpaceBefore, TEST_UNIT_IN, TEST_UNIT_OUT_CALL);
end;

procedure TTestSpaceBeforeBrackets.TestSpaceBeforeBrackets_InBoth;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := True;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := True;
  TestProcessResult(TSingleSpaceBefore, TEST_UNIT_IN, TEST_UNIT_OUT_BOTH);
end;

const
  TEST_ARRAY_IN = UNIT_HEADER +
  'function foo(value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + bar[1];' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;

  TEST_ARRAY_OUT = UNIT_HEADER +
  'function foo(value: integer): integer;' + AnsiLineBreak +
  'begin' + AnsiLineBreak +
  '  result := 1 + bar [1];' +  AnsiLineBreak +
  'end;' + AnsiLineBreak +
  UNIT_FOOTER;

procedure TTestSpaceBeforeBrackets.TestSpaceBeforeArray_Off;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := False;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := False;
  FormatSettings.Spaces.SpaceBeforeOpenSquareBracketsInExpression := False;

  TestProcessResult(TSingleSpaceBefore, TEST_ARRAY_IN, TEST_ARRAY_IN);
end;

procedure TTestSpaceBeforeBrackets.TestSpaceBeforeArray_On;
begin
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration := False;
  FormatSettings.Spaces.SpaceBeforeOpenBracketsInFunctionCall := False;
  FormatSettings.Spaces.SpaceBeforeOpenSquareBracketsInExpression := True;

  TestProcessResult(TSingleSpaceBefore, TEST_ARRAY_IN, TEST_ARRAY_OUT);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestSpaceBeforeBrackets.Suite);
end.
