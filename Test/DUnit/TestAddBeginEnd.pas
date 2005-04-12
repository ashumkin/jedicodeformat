unit TestAddBeginEnd;

interface

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAddBeginEnd = class(TBaseTestProcess)
  private
    feSaveBeginEndStyle: TTriOptionStyle;
  protected
    procedure Setup; override;
    procedure Teardown; override;
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
  end;

implementation

{ TTestAddBeginEnd }

uses
  JclStrings,
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
  UNIT_HEADER = 'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface ' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak;

  UNIT_FOOTER = AnsiLineBreak + 'end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

  IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;

  IF_ELSE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    ShowMessage(''big'')' +
    ' else ' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_ELSE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end' +
    ' else ' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;

  DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    if i > 20 then ' + AnsiLineBreak +
    '      ShowMessage(''big'');' +
    UNIT_FOOTER;

  DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 10 then ' + AnsiLineBreak +
    '    begin if i > 20 then ' + AnsiLineBreak +
    '      begin ShowMessage(''big'') end end;' +
    UNIT_FOOTER;

  WHILE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  while i > 10 do ' + AnsiLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  WHILE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  while i > 10 do ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;


  FOR_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  for i := 1 to 3 do ' + AnsiLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  FOR_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  for i := 1 to 3 do ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;


  WITH_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  with i do ' + AnsiLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

  WITH_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  with i do ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' +
    UNIT_FOOTER;

  CASE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  case i of ' + AnsiLineBreak +
    '    1: ShowMessage(''one'');' +
    '    2: ShowMessage(''two'');' +
    '    else ShowMessage(''lots'');' +
    '  end ' +
    UNIT_FOOTER;

  CASE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  case i of ' + AnsiLineBreak +
    '    1: begin ShowMessage(''one''); end;' +
    '    2: begin ShowMessage(''two''); end;' +
    '    else begin ShowMessage(''lots''); end;' +
    '  end ' +
    UNIT_FOOTER;


  IF_FOR_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    for i := 1 to 3 do ' + AnsiLineBreak +
    '      ShowMessage(''big'');' +
    UNIT_FOOTER;

  IF_FOR_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    begin for i := 1 to 3 do ' + AnsiLineBreak +
    '      begin ShowMessage(''big'') end; end;' +
    UNIT_FOOTER;

  IF_FOR_ELSE_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    for i := 1 to 3 do ' + AnsiLineBreak +
    '      ShowMessage(''big'')' +
    '  else' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_FOR_ELSE_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    begin for i := 1 to 3 do ' + AnsiLineBreak +
    '      begin ShowMessage(''big'') end end' +
    '  else' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;

  IF_FOR_ELSE_IF_STATEMENT_TEXT_NO_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    for i := 1 to 3 do ' + AnsiLineBreak +
    '      ShowMessage(''big'')' +
    '  else if i > 2 then' +
    '    ShowMessage(''small'');' +
    UNIT_FOOTER;

  IF_FOR_ELSE_IF_STATEMENT_TEXT_WITH_BEGIN =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    begin for i := 1 to 3 do ' + AnsiLineBreak +
    '      begin ShowMessage(''big'') end end' +
    '  else if i > 2 then' +
    '    begin ShowMessage(''small'') end;' +
    UNIT_FOOTER;


  { in this case removing the begin..end is wrong
    because it causes the else to attach to the inner if
    Thus changing the program meaning }
  NESTED_IF_TEXT_WITH_BEGIN1 =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '  begin' + AnsiLineBreak +
    '    if i > 5 then' + AnsiLineBreak +
    '      ShowMessage(''bigger'')' + AnsiLineBreak +
    '  end' + AnsiLineBreak +
    '  else' + AnsiLineBreak +
    '    ShowMessage(''big'');' +
    UNIT_FOOTER;

    NESTED_IF_WITH_ALL_BEGINS1 =
      UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '  begin'+ AnsiLineBreak +
    '    if i > 5 then' + AnsiLineBreak +
    '      begin ShowMessage(''bigger'') end' + AnsiLineBreak +
    '  end' + AnsiLineBreak +
    '  else' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' + 
    UNIT_FOOTER;

  NESTED_IF_TEXT_WITH_BEGIN2 =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    begin if i > 5 then' + AnsiLineBreak +
    '     begin ShowMessage(''bigger'') end' + AnsiLineBreak +
    '    else' + AnsiLineBreak +
    '     begin ShowMessage(''big'') end end;' +
    UNIT_FOOTER;

  NESTED_IF_TEXT_NO_BEGIN2 =
    UNIT_HEADER +
    '  if i > 3 then' + AnsiLineBreak +
    '    if i > 5 then' + AnsiLineBreak +
    '     ShowMessage(''bigger'')' + AnsiLineBreak +
    '    else' + AnsiLineBreak +
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
    '  if i > 3 then' + AnsiLineBreak +
    '  begin' + AnsiLineBreak +
    '    if i > 5 then' + AnsiLineBreak +
    '      ShowMessage(''foo'')' + AnsiLineBreak +
    '    else if Condition_C then' + AnsiLineBreak +
    '      ShowMessage(''fish'')' + AnsiLineBreak +
    '  end' + AnsiLineBreak +
    '  else' + AnsiLineBreak +
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

initialization
  TestFramework.RegisterTest('Processes', TTestAddBeginEnd.Suite);

end.
