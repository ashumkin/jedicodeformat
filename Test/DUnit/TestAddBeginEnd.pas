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
  IF_STATEMENT_TEXT_NO_BEGIN =
    'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface ' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    '  if i > 10 then ' + AnsiLineBreak +
    '    ShowMessage(''big'');' + AnsiLineBreak +
    'end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

  IF_STATEMENT_TEXT_WITH_BEGIN =
    'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface ' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    '  if i > 10 then ' + AnsiLineBreak +
    '    begin ShowMessage(''big'') end;' + AnsiLineBreak +
    'end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';


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


initialization
  TestFramework.RegisterTest('Processes', TTestAddBeginEnd.Suite);

end.
