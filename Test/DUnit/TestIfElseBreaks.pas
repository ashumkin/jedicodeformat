unit TestIfElseBreaks;

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess;

type
  TTestIfElseBreaks = class(TBaseTestProcess)
  private

  protected
  published
    procedure TestIfElseRemoveReturn1;
    procedure TestIfElseRemoveReturn2;
    procedure TestIfElseAddReturn1;
    procedure TestIfElseAddReturn2;
    procedure TestIfElseLeaveReturnAsIs1;
    procedure TestIfElseLeaveReturnAsIs2;
  end;

implementation

uses JclStrings, BlockStyles, JcfSettings, SettingsTypes;

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

initialization
 TestFramework.RegisterTest(TTestIfElseBreaks.Suite);
end.
