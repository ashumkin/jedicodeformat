unit TestAddBeginEnd;

interface

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAddBeginEnd = class(TBaseTestProcess)
  private
    feSaveBeginEndStyle: TBlockBeginEnd;
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
    '      begin ShowMessage(''big'') end; end;' +
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


procedure TTestAddBeginEnd.TestRemoveFromIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, IF_STATEMENT_TEXT_WITH_BEGIN,
    IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebAlways;

  TestProcessResult(TAddBeginEnd, IF_STATEMENT_TEXT_NO_BEGIN,
    IF_STATEMENT_TEXT_WITH_BEGIN);
end;


procedure TTestAddBeginEnd.TestAddToIfElseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebAlways;

  TestProcessResult(TAddBeginEnd, IF_ELSE_STATEMENT_TEXT_NO_BEGIN,
    IF_ELSE_STATEMENT_TEXT_WITH_BEGIN);

end;

procedure TTestAddBeginEnd.TestRemoveFromIfElseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, IF_ELSE_STATEMENT_TEXT_WITH_BEGIN,
    IF_ELSE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToDoubleIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN,
    DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromDoubleIfStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, DOUBLE_IF_STATEMENT_TEXT_WITH_BEGIN,
    DOUBLE_IF_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToWhileStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, WHILE_STATEMENT_TEXT_WITH_BEGIN,
    WHILE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromWhileStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, WHILE_STATEMENT_TEXT_WITH_BEGIN,
    WHILE_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, FOR_STATEMENT_TEXT_WITH_BEGIN,
    FOR_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromForStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, FOR_STATEMENT_TEXT_WITH_BEGIN,
    FOR_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestAddToWithStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromWithStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;


procedure TTestAddBeginEnd.TestAddToCaseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, WITH_STATEMENT_TEXT_WITH_BEGIN,
    WITH_STATEMENT_TEXT_NO_BEGIN);
end;

procedure TTestAddBeginEnd.TestRemoveFromCaseStatement;
begin
  FormatSettings.Transform.BeginEndStyle := ebNever;

  TestProcessResult(TAddBeginEnd, CASE_STATEMENT_TEXT_WITH_BEGIN,
    CASE_STATEMENT_TEXT_NO_BEGIN);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAddBeginEnd.Suite);

end.
