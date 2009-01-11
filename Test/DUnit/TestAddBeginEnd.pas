unit TestAddBeginEnd;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAddBeginEnd
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAddBeginEnd = class(TBaseTestProcess)
  private
    feSaveBeginEndStyle: TTriOptionStyle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure TestAddToIfStatement;
    procedure TestRemoveFromIfStatement;

    procedure TestAddToIfElseStatement;
    procedure TestRemoveFromIfElseStatement;

    procedure TestAddToDoubleIfStatement;
    procedure TestRemoveFromDoubleIfStatement;

    procedure TestAddToWhileStatement;
    procedure TestRemoveFromWhileStatement;

    procedure TestAddToForStatement;
    procedure TestRemoveFromForStatement;

    procedure TestAddToWithStatement;
    procedure TestRemoveFromWithStatement;

    procedure TestAddToCaseStatement;
    procedure TestRemoveFromCaseStatement;

    procedure TestAddToifForStatement;
    procedure TestAddToifForStatement2;
    procedure TestAddToifForStatement3;
    procedure TestRemoveFromIfForStatement;

    procedure TestNestedIf1;
    procedure TestNestedIf1_2;
    procedure TestNestedIf1_3;

    procedure TestNestedIf2;
    procedure TestNestedIf3;
    procedure TestNestedIf4;

    procedure TestBug1174572;
    procedure TestBug1262542;

    procedure TestBugNew;
  end;

implementation

uses
  JcfStringUtils,
  JcfSettings, AddBeginEnd;

procedure TTestAddBeginEnd.Setup;
begin
  inherited;
  feSaveBeginEndStyle := FormatSettings.Transform.BeginEndStyle;
end;

procedure TTestAddBeginEnd.Teardown;
begin
  inherited;

  FormatSettings.Transform.BeginEndStyle := feSaveBeginEndStyle;
end;

const
  UNIT_HEADER = 'unit CaseTest;' + NativeLineBreak + NativeLineBreak +
    'interface ' + NativeLineBreak + NativeLineBreak +
    'implementation' + NativeLineBreak + NativeLineBreak +
    'uses Dialogs;' + NativeLineBreak + NativeLineBreak +
    'procedure foo(i: integer);' + NativeLineBreak +
    'begin' + NativeLineBreak;

  UNIT_FOOTER = NativeLineBreak + 'end;' + NativeLineBreak + NativeLineBreak +
    'end.';

  IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;

  IF_ELSE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    ShowMessage(''big'')' +
    ' else ' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_ELSE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    begin ShowMessage(''big'') end' +
    ' else ' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;

  DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    if i > 20 then ' + NativeLineBreak +
    '      ShowMessage(''big'');' +
    UNIT_FOOTER;

  DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + NativeLineBreak +
    '    begin if i > 20 then ' + NativeLineBreak +
    '      begin ShowMessage(''big'') end end;' +
    UNIT_FOOTER;

  WHILE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  while i > 10 do ' + NativeLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  WHILE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  while i > 10 do ' + NativeLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;


  FOR_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  for i := 1 to 3 do ' + NativeLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  FOR_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  for i := 1 to 3 do ' + NativeLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;


  WITH_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  with i do ' + NativeLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  WITH_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  with i do ' + NativeLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;

  CASE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  case i of ' + NativeLineBreak +
    '    1: ShowMessage(''one'');' +
    '    2: ShowMessage(''two'');' +
    '    else ShowMessage(''lots'');' +
    '  end ' +
    UNIT_FOOTER;

  CASE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  case i of ' + NativeLineBreak +
    '    1: begin ShowMessage(''one''); end;' +
    '    2: begin ShowMessage(''two''); end;' +
    '    else begin ShowMessage(''lots''); end;' +
    '  end ' +
    UNIT_FOOTER;


  IF_FOR_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    for i := 1 to 3 do ' + NativeLineBreak +
    '      ShowMessage(''big'');' +
    UNIT_FOOTER;

  IF_FOR_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    begin for i := 1 to 3 do ' + NativeLineBreak +
    '      begin ShowMessage(''big'') end; end;' +
    UNIT_FOOTER;

  IF_FOR_ELSE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    for i := 1 to 3 do ' + NativeLineBreak +
    '      ShowMessage(''big'')' +
    '  else' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_FOR_ELSE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    begin for i := 1 to 3 do ' + NativeLineBreak +
    '      begin ShowMessage(''big'') end end' +
    '  else' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;

  IF_FOR_ELSE_IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    for i := 1 to 3 do ' + NativeLineBreak +
    '      ShowMessage(''big'')' +
    '  else if i > 2 then' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_FOR_ELSE_IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    begin for i := 1 to 3 do ' + NativeLineBreak +
    '      begin ShowMessage(''big'') end end' +
    '  else if i > 2 then' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;


  { in this case removing the begin..end is wrong
    because it causes the else to attach to the inner if
    Thus changing the program meaning }
  NESTED_IF_TEXT_WITH_BEGIN1 =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    if i > 5 then' + NativeLineBreak +
    '      ShowMessage(''bigger'')' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

    NESTED_IF_WITH_ALL_BEGINS1 =
      UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '  begin'+ NativeLineBreak +
    '    if i > 5 then' + NativeLineBreak +
    '      begin ShowMessage(''bigger'') end' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    begin ShowMessage(''big'') end;' + 
    UNIT_FOOTER;

  NESTED_IF_TEXT_WITH_BEGIN2 =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    begin if i > 5 then' + NativeLineBreak +
    '     begin ShowMessage(''bigger'') end' + NativeLineBreak +
    '    else' + NativeLineBreak +
    '     begin ShowMessage(''big'') end end;' +
    UNIT_FOOTER;

  NESTED_IF_TEXT_NO_BEGIN2 =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    if i > 5 then' + NativeLineBreak +
    '     ShowMessage(''bigger'')' + NativeLineBreak +
    '    else' + NativeLineBreak +
    '     ShowMessage(''big'');' +
    UNIT_FOOTER;


procedure TTestAddBeginEnd.TestRemoveFromIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, IF_STATEMENT_TEXT_WITH_BEGIN,
    IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, IF_STATEMENT_TEXT_NO_BEGIN,
    IF_STATEMENT_TEXT_WITH_BEGIN);
end;


procedure TTestAddBeginEnd.TestAddToIfElseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, IF_ELSE_STATEMENT_TEXT_NO_BEGIN,
    IF_ELSE_STATEMENT_TEXT_WITH_BEGIN);

end;

procedure TTestAddBeginEnd.TestRemoveFromIfElseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, IF_ELSE_STATEMENT_TEXT_WITH_BEGIN,
    IF_ELSE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToDoubleIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN,
    DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromDoubleIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN,
    DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToWhileStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, WHILE_STATEMENT_TEXT_WITH_BEGIN,
    WHILE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromWhileStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, WHILE_STATEMENT_TEXT_WITH_BEGIN,
    WHILE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, FOR_STATEMENT_TEXT_WITH_BEGIN,
    FOR_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, FOR_STATEMENT_TEXT_WITH_BEGIN,
    FOR_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToWithStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromWithStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;


procedure TTestAddBeginEnd.TestAddToCaseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromCaseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, CASE_STATEMENT_TEXT_WITH_BEGIN,
    CASE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToIfForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, IF_FOR_STATEMENT_TEXT_WITH_BEGIN,
    IF_FOR_STATEMENT_TEXT_NO_BEGIN);
end;


procedure TTestAddBeginEnd.TestAddToifForStatement2;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, IF_FOR_ELSE_STATEMENT_TEXT_NO_BEGIN,
    IF_FOR_ELSE_STATEMENT_TEXT_WITH_BEGIN);
end;

// note that the "else..if" doesn't become "else begin if"
procedure TTestAddBeginEnd.TestAddToifForStatement3;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, IF_FOR_ELSE_IF_STATEMENT_TEXT_NO_BEGIN,
    IF_FOR_ELSE_IF_STATEMENT_TEXT_WITH_BEGIN);
end;


procedure TTestAddBeginEnd.TestRemoveFromIfForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, IF_FOR_STATEMENT_TEXT_WITH_BEGIN,
    IF_FOR_STATEMENT_TEXT_NO_BEGIN);
end;


{ it's not alays safe to add or remove begin..end from nested if statements
e.g.

if a > 1 then
begin
  if a > 2 then
    Foo
end
else
  Bar;


  is not the same as

if a > 1 then
  if a > 2 then
    Foo
else
  Bar;

}
procedure TTestAddBeginEnd.TestNestedIf1;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, NESTED_IF_TEXT_WITH_BEGIN1,
    NESTED_IF_TEXT_WITH_BEGIN1);
end;


procedure TTestAddBeginEnd.TestNestedIf1_2;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, NESTED_IF_TEXT_WITH_BEGIN1,
    NESTED_IF_WITH_ALL_BEGINS1);
end;


procedure TTestAddBeginEnd.TestNestedIf1_3;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, NESTED_IF_WITH_ALL_BEGINS1,
    NESTED_IF_TEXT_WITH_BEGIN1);
end;

procedure TTestAddBeginEnd.TestNestedIf2;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;
  TestProcessResult(TAddBeginEnd, NESTED_IF_TEXT_NO_BEGIN2,
    NESTED_IF_TEXT_WITH_BEGIN2);
end;

procedure TTestAddBeginEnd.TestNestedIf3;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, NESTED_IF_TEXT_WITH_BEGIN2,
    NESTED_IF_TEXT_NO_BEGIN2);
end;

procedure TTestAddBeginEnd.TestNestedIf4;
begin
  FormatSettings.Transform.BeginEndStyle := eAlways;

  TestProcessResult(TAddBeginEnd, NESTED_IF_TEXT_NO_BEGIN2, NESTED_IF_TEXT_WITH_BEGIN2);
end;


const
  TEST_1174572_IN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    if i > 5 then' + NativeLineBreak +
    '      ShowMessage(''foo'')' + NativeLineBreak +
    '    else if Condition_C then' + NativeLineBreak +
    '      ShowMessage(''fish'')' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    ShowMessage(''spon'');' +
    UNIT_FOOTER;


procedure TTestAddBeginEnd.TestBug1174572;
begin
  { sourceforge bug [1174572 ]
    Remove begin and end from around single statement bug
   Removing the begin-end changes the meaning and should not be done }
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, TEST_1174572_IN, TEST_1174572_IN);
end;

const
  TEST_1262542_IN =
    UNIT_HEADER +
    '  if i > 3 then' + NativeLineBreak +
    '    if i > 5 then' + NativeLineBreak +
    '      ShowMessage(''foo'')' + NativeLineBreak +
    '    else' + NativeLineBreak  +
    '    begin' + NativeLineBreak +
    '      if Condition_C then' + NativeLineBreak +
    '         ShowMessage(''fish'')' + NativeLineBreak +
    '     end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    ShowMessage(''spon'');' +
    UNIT_FOOTER;

procedure TTestAddBeginEnd.TestBug1262542;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, TEST_1262542_IN, TEST_1262542_IN);
end;

const
  TEST_NEW_IN =
    UNIT_HEADER +
    '  if B then' +
    '  begin' +
    '    for i := 0 to 10 do' +
    '      if C then' +
    '        if D then' +
    '          dddd' +
    '        else' +
    '        if E then' +
    '          eeee' +
    '  end' +
    '  else' +
    '    for i := 0 to 10 do' +
    '      if F then' +
    '        ffff;' +
      UNIT_FOOTER;

procedure TTestAddBeginEnd.TestBugNew;
begin
  FormatSettings.Transform.BeginEndStyle := eNever;

  TestProcessResult(TAddBeginEnd, TEST_NEW_IN, TEST_NEW_IN);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAddBeginEnd.Suite);

end.
