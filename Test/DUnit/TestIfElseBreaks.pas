unit TestIfElseBreaks;

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess, SettingsTypes;

type
  TTestIfElseBreaks = class(TBaseTestProcess)
  private
    leSaveIfElseStyle, leSaveBareBlockStyle: TBlockNewLineStyle;

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
end;

procedure TTestIfElseBreaks.Teardown;
begin
  inherited;

  FormatSettings.Returns.ElseIfStyle := leSaveIfElseStyle;
  FormatSettings.Returns.BlockStyle := leSaveBareBlockStyle;
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
 TestFramework.RegisterTest('Processes', TTestIfElseBreaks.Suite);
end.
