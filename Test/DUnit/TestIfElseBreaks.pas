unit TestIfElseBreaks;

interface

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes;

type
  TTestIfElseBreaks = class(TBaseTestProcess)
  private
    leSaveIfElseStyle, leSaveBareBlockStyle: TBlockNewLineStyle;
    leSaveCaseLabelStyle, leSaveCaseElseStyle: TBlockNewLineStyle;

  protected
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestIfElseRemoveReturn1;
    procedure TestIfElseRemoveReturn2;
    procedure TestIfElseAddReturn1;
    procedure TestIfElseAddReturn2;
    procedure TestIfElseLeaveReturnAsIs1;
    procedure TestIfElseLeaveReturnAsIs2;

    procedure TestBlockStyleNever;
    procedure TestBlockStyleNeverWithComment;

    procedure TestIfElseStyleNever;
    procedure TestIfElseNeverWithComment;

    procedure TestCaseStatementNever1;
    procedure TestCaseStatementNever2;
    procedure TestCaseStatementLeaveAsIs1;
    procedure TestCaseStatementLeaveAsIs2;
    procedure TestCaseStatementAlways1;
    procedure TestCaseStatementAlways2;
  end;

implementation

uses JclStrings, BlockStyles, JcfSettings;

const

  RETURN_ADDED_TEXT =
    'unit TestIfElseBreak;' + AnsiLineBreak +
    AnsiLineBreak +
    'interface' + AnsiLineBreak +
    AnsiLineBreak +
    'implementation' + AnsiLineBreak +
    AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak +
    AnsiLineBreak +
    'procedure TestBreaks;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'');'+ AnsiLineBreak +
    AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else'+ AnsiLineBreak +
    '  ShowMessage(''false'');'+ AnsiLineBreak +
    AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else'+ AnsiLineBreak +
    ' if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else' + AnsiLineBreak +
    '  ShowMessage(''false'');' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    AnsiLineBreak +
    'end.';

  RETURN_REMOVED_TEXT =
    'unit TestIfElseBreak;' + AnsiLineBreak +
    AnsiLineBreak +
    'interface' + AnsiLineBreak +
    AnsiLineBreak +
    'implementation' + AnsiLineBreak +
    AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak +
    AnsiLineBreak +
    'procedure TestBreaks;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'');'+ AnsiLineBreak +
    AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else'+ AnsiLineBreak +
    '  ShowMessage(''false'');'+ AnsiLineBreak +
    AnsiLineBreak +
    'if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else if True then'+ AnsiLineBreak +
    '  ShowMessage(''twoo'')'+ AnsiLineBreak +
    'else' + AnsiLineBreak +
    '  ShowMessage(''false'');' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    AnsiLineBreak +
    'end.';

    { TTestIfElseBreaks }

procedure TTestIfElseBreaks.Setup;
begin
  inherited;

  leSaveIfElseStyle := FormatSettings.Returns.ElseIfStyle;
  leSaveBareBlockStyle := FormatSettings.Returns.BlockStyle;

  leSaveCaseLabelStyle := FormatSettings.Returns.CaseLabelStyle;
  leSaveCaseElseStyle := FormatSettings.Returns.CaseElseStyle;
end;

procedure TTestIfElseBreaks.Teardown;
begin
  inherited;

  FormatSettings.Returns.ElseIfStyle := leSaveIfElseStyle;
  FormatSettings.Returns.BlockStyle := leSaveBareBlockStyle;

  FormatSettings.Returns.CaseLabelStyle := leSaveCaseLabelStyle;
  FormatSettings.Returns.CaseElseStyle := leSaveCaseElseStyle;
end;

procedure TTestIfElseBreaks.TestBlockStyleNever;
const
  IN_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then ' + AnsiLineBreak +
    ' Fish();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then  Fish();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  FormatSettings.Returns.BlockStyle := eNever;

  TestProcessResult(TBlockStyles, IN_TEXT, OUT_TEXT);
end;

procedure TTestIfElseBreaks.TestBlockStyleNeverWithComment;
const
  IN_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then // noremove' + AnsiLineBreak +
    ' Fish();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  FormatSettings.Returns.BlockStyle := eNever;

  TestProcessResult(TBlockStyles, IN_TEXT, IN_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseStyleNever;
const
  IN_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else if spon then' + AnsiLineBreak +
    ' Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then Fish()' + AnsiLineBreak +
    'else if spon then Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  FormatSettings.Returns.BlockStyle := eNever;

  TestProcessResult(TBlockStyles, IN_TEXT, OUT_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseNeverWithComment;
const
  IN_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then // comment' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else if spon then // comment' + AnsiLineBreak +
    ' Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin'  + AnsiLineBreak +
    'if bar then // comment' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else if spon then // comment' + AnsiLineBreak +
    ' Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  FormatSettings.Returns.BlockStyle := eNever;

  TestProcessResult(TBlockStyles, IN_TEXT, OUT_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseAddReturn1;
begin
  FormatSettings.Returns.ElseIfStyle := eAlways;
  TestProcessResult(TBlockStyles, RETURN_REMOVED_TEXT, RETURN_ADDED_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseAddReturn2;
begin
  FormatSettings.Returns.ElseIfStyle := eAlways;
  TestProcessResult(TBlockStyles, RETURN_ADDED_TEXT, RETURN_ADDED_TEXT);
end;


procedure TTestIfElseBreaks.TestIfElseLeaveReturnAsIs1;
begin
  FormatSettings.Returns.ElseIfStyle := eLeave;
  TestProcessResult(TBlockStyles, RETURN_ADDED_TEXT, RETURN_ADDED_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseLeaveReturnAsIs2;
begin
  FormatSettings.Returns.ElseIfStyle := eLeave;
  TestProcessResult(TBlockStyles, RETURN_REMOVED_TEXT, RETURN_REMOVED_TEXT);
end;


procedure TTestIfElseBreaks.TestIfElseRemoveReturn1;
begin
  FormatSettings.Returns.ElseIfStyle := eNever;
  TestProcessResult(TBlockStyles, RETURN_ADDED_TEXT, RETURN_REMOVED_TEXT);
end;

procedure TTestIfElseBreaks.TestIfElseRemoveReturn2;
begin
  FormatSettings.Returns.ElseIfStyle := eNever;
  TestProcessResult(TBlockStyles, RETURN_REMOVED_TEXT, RETURN_REMOVED_TEXT);
end;

const
  CASE_STATEMENT_IN_TEXT_NO_BREAKS =
    'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface ' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    '  case i of' + AnsiLineBreak +
    '    1: ShowMessage(''1 ... OK'');' + AnsiLineBreak +
    '    else ShowMessage(''else ... OK'');' + AnsiLineBreak +
    '   end;' +  AnsiLineBreak +
    '  end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

    CASE_STATEMENT_IN_TEXT_BREAKS =
    'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface ' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    '  case i of' + AnsiLineBreak +
    '    1:' + AnsiLineBreak +
    ' ShowMessage(''1 ... OK'');' + AnsiLineBreak +
    '    else' + AnsiLineBreak +
    ' ShowMessage(''else ... OK'');' + AnsiLineBreak +
    '   end;' +  AnsiLineBreak +
    '  end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

procedure TTestIfElseBreaks.TestCaseStatementNever1;
begin
  FormatSettings.Returns.CaseLabelStyle := eNever;
  FormatSettings.Returns.CaseElseStyle := eNever;

  // no breaks - text without breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS, CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementNever2;
begin
  FormatSettings.Returns.CaseLabelStyle := eNever;
  FormatSettings.Returns.CaseElseStyle := eNever;

  // no breaks - text with breaks is altered
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS, CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementLeaveAsIs1;
begin
  FormatSettings.Returns.CaseLabelStyle := eLeave;
  FormatSettings.Returns.CaseElseStyle := eLeave;

  // leave as is - text with no breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS, CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementLeaveAsIs2;
begin
  FormatSettings.Returns.CaseLabelStyle := eLeave;
  FormatSettings.Returns.CaseElseStyle := eLeave;

  // leave as is - text with breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS, CASE_STATEMENT_IN_TEXT_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementAlways1;
begin
  FormatSettings.Returns.CaseLabelStyle := eAlways;
  FormatSettings.Returns.CaseElseStyle := eAlways;

  // breaks - text without breaks has them inserted
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS, CASE_STATEMENT_IN_TEXT_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementAlways2;
begin
  FormatSettings.Returns.CaseLabelStyle := eAlways;
  FormatSettings.Returns.CaseElseStyle := eAlways;

  // breaks - text with breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS, CASE_STATEMENT_IN_TEXT_BREAKS);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestIfElseBreaks.Suite);
end.
