unit TestWarnings;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TestWarnings, released May 2003.
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
  TestFrameWork, BaseTestProcess,
  StringsConverter;

type
  TTestWarnings = class(TBaseTestProcess)
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
    procedure TestCaseNoElse2;

 end;


implementation

uses SysUtils, JclStrings;

const
  EMPTY_BEGIN_END = 'Empty begin..end block';
  EMPTY_TRY = 'Empty try block';
  EMPTY_EXCEPT_END = 'Empty except..end';
  EMPTY_FINALLY_END  = 'Empty finally..end';
  REAL_TYPE_USED = 'Real type used';
  REAL48_TYPE_USED = 'Real48 type used';


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

procedure TTestWarnings.TestCaseNoElse2;
const
  // this one has an else, should have no warning
  UNIT_TEXT = UNIT_HEADER +  'procedure fred; var li: integer; begin case li of 1: ; else; end; end; ' + UNIT_FOOTER;
begin
  TestNoWarnings(UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest(TTestWarnings.Suite);
end.