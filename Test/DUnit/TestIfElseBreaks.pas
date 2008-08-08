unit TestIfElseBreaks;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestIfElseBreaks
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
  BaseTestProcess, SettingsTypes;

type
  TTestIfElseBreaks = class(TBaseTestProcess)
  private
    leSaveIfElseStyle, leSaveBareBlockStyle: TTriOptionStyle;
    leSaveCaseLabelStyle, leSaveCaseElseStyle: TTriOptionStyle;
    leSaveEndElseStyle: TTriOptionStyle;
    leSaveElseBeginStyle: TTriOptionStyle;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
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

    procedure TestEndElseStyle1;
    procedure TestEndElseStyle2;
    procedure TestEndElseStyle3;
    procedure TestEndElseStyle4;
    procedure TestEndElseStyle5;
    procedure TestEndElseStyle6;

    procedure TestAddElseBegin1;
    procedure TestAddElseBegin2;
    procedure TestRemoveElseBegin1;
    procedure TestRemoveElseBegin2;
  end;

implementation

uses JclAnsiStrings, BlockStyles, JcfSettings, SetReturns;

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
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'');' + AnsiLineBreak +
    AnsiLineBreak +
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
    'else' + AnsiLineBreak +
    '  ShowMessage(''false'');' + AnsiLineBreak +
    AnsiLineBreak +
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
    'else' + AnsiLineBreak +
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
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
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'');' + AnsiLineBreak +
    AnsiLineBreak +
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
    'else' + AnsiLineBreak +
    '  ShowMessage(''false'');' + AnsiLineBreak +
    AnsiLineBreak +
    'if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
    'else if True then' + AnsiLineBreak +
    '  ShowMessage(''twoo'')' + AnsiLineBreak +
    'else' + AnsiLineBreak +
    '  ShowMessage(''false'');' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    AnsiLineBreak +
    'end.';

    { TTestIfElseBreaks }

procedure TTestIfElseBreaks.Setup;
begin
  inherited;

  leSaveIfElseStyle    := FormatSettings.Returns.ElseIfStyle;
  leSaveBareBlockStyle := FormatSettings.Returns.BlockStyle;

  leSaveCaseLabelStyle := FormatSettings.Returns.CaseLabelStyle;
  leSaveCaseElseStyle  := FormatSettings.Returns.CaseElseStyle;

  leSaveEndElseStyle := FormatSettings.Returns.EndElseStyle;
  leSaveElseBeginStyle := FormatSettings.Returns.ElseBeginStyle;

  FormatSettings.Returns.ElseBeginStyle := eLeave;
end;

procedure TTestIfElseBreaks.Teardown;
begin
  inherited;

  FormatSettings.Returns.ElseIfStyle := leSaveIfElseStyle;
  FormatSettings.Returns.BlockStyle  := leSaveBareBlockStyle;

  FormatSettings.Returns.CaseLabelStyle := leSaveCaseLabelStyle;
  FormatSettings.Returns.CaseElseStyle  := leSaveCaseElseStyle;

  FormatSettings.Returns.EndElseStyle := leSaveEndElseStyle;
  FormatSettings.Returns.ElseBeginStyle := leSaveElseBeginStyle;
end;

procedure TTestIfElseBreaks.TestBlockStyleNever;
const
  IN_TEXT  = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if bar then ' + AnsiLineBreak +
    ' Fish();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
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
    'begin' + AnsiLineBreak +
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
  IN_TEXT  = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if bar then' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else if spon then' + AnsiLineBreak +
    ' Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
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
  IN_TEXT  = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if bar then // comment' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else if spon then // comment' + AnsiLineBreak +
    ' Wibble();' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_TEXT = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
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
    '   end;' + AnsiLineBreak +
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
    'ShowMessage(''1 ... OK'');' + AnsiLineBreak +
    '    else' + AnsiLineBreak +
    'ShowMessage(''else ... OK'');' + AnsiLineBreak +
    '   end;' + AnsiLineBreak +
    '  end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

procedure TTestIfElseBreaks.TestCaseStatementNever1;
begin
  FormatSettings.Returns.CaseLabelStyle := eNever;
  FormatSettings.Returns.CaseElseStyle  := eNever;

  // no breaks - text without breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS,
    CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementNever2;
begin
  FormatSettings.Returns.CaseLabelStyle := eNever;
  FormatSettings.Returns.CaseElseStyle  := eNever;

  // no breaks - text with breaks is altered
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS,
    CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementLeaveAsIs1;
begin
  FormatSettings.Returns.CaseLabelStyle := eLeave;
  FormatSettings.Returns.CaseElseStyle  := eLeave;

  // leave as is - text with no breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS,
    CASE_STATEMENT_IN_TEXT_NO_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementLeaveAsIs2;
begin
  FormatSettings.Returns.CaseLabelStyle := eLeave;
  FormatSettings.Returns.CaseElseStyle  := eLeave;

  // leave as is - text with breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS,
    CASE_STATEMENT_IN_TEXT_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementAlways1;
begin
  FormatSettings.Returns.CaseLabelStyle := eAlways;
  FormatSettings.Returns.CaseElseStyle  := eAlways;

  // breaks - text without breaks has them inserted
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_NO_BREAKS,
    CASE_STATEMENT_IN_TEXT_BREAKS);
end;

procedure TTestIfElseBreaks.TestCaseStatementAlways2;
begin
  FormatSettings.Returns.CaseLabelStyle := eAlways;
  FormatSettings.Returns.CaseElseStyle  := eAlways;

  // breaks - text with breaks is left as is
  TestProcessResult(TBlockStyles, CASE_STATEMENT_IN_TEXT_BREAKS,
    CASE_STATEMENT_IN_TEXT_BREAKS);
end;

const
  BROKEN_END_ELSE_UNIT_TEXT =
   'unit TestCase;' + AnsiLineBreak +
   'interface' + AnsiLineBreak +
   'implementation' + AnsiLineBreak +
   AnsiLineBreak +
   'procedure foo;' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'if (a > b) then' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'end' + AnsiLineBreak +
   'else' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'end;' + AnsiLineBreak +
   'end;' + AnsiLineBreak +
   'end.';

  UNBROKEN_END_ELSE_UNIT_TEXT =
   'unit TestCase;' + AnsiLineBreak +
   'interface' + AnsiLineBreak +
   'implementation' + AnsiLineBreak +
   AnsiLineBreak +
   'procedure foo;' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'if (a > b) then' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'end else' + AnsiLineBreak +
   'begin' + AnsiLineBreak +
   'end;' + AnsiLineBreak +
   'end;' + AnsiLineBreak +
   'end.';

procedure TTestIfElseBreaks.TestEndElseStyle1;
begin
  FormatSettings.Returns.EndElseStyle := eLeave;
  TestProcessResult(TBlockStyles, BROKEN_END_ELSE_UNIT_TEXT, BROKEN_END_ELSE_UNIT_TEXT);
end;


procedure TTestIfElseBreaks.TestEndElseStyle2;
begin
  FormatSettings.Returns.EndElseStyle := eLeave;
  TestProcessResult(TBlockStyles, UNBROKEN_END_ELSE_UNIT_TEXT, UNBROKEN_END_ELSE_UNIT_TEXT);
end;

procedure TTestIfElseBreaks.TestEndElseStyle3;
begin
  FormatSettings.Returns.EndElseStyle := eAlways;
  // breaks - text with breaks is left as is
  TestProcessResult(TBlockStyles, BROKEN_END_ELSE_UNIT_TEXT, BROKEN_END_ELSE_UNIT_TEXT);
end;

procedure TTestIfElseBreaks.TestEndElseStyle4;
begin
  FormatSettings.Returns.EndElseStyle := eAlways;
  // without breaks -> breaks added
  TestProcessResult(TBlockStyles, UNBROKEN_END_ELSE_UNIT_TEXT, BROKEN_END_ELSE_UNIT_TEXT);
end;

procedure TTestIfElseBreaks.TestEndElseStyle5;
begin
  FormatSettings.Returns.EndElseStyle := eNever;

  TestProcessResult(TBlockStyles, BROKEN_END_ELSE_UNIT_TEXT, UNBROKEN_END_ELSE_UNIT_TEXT);
end;

procedure TTestIfElseBreaks.TestEndElseStyle6;
begin
  FormatSettings.Returns.EndElseStyle := eNever;
  TestProcessResult(TBlockStyles, UNBROKEN_END_ELSE_UNIT_TEXT, UNBROKEN_END_ELSE_UNIT_TEXT);
end;

const
  ELSE_BEGIN_TEXT_NO_RETURN  = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if bar then // comment' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else begin ' + AnsiLineBreak +
    '   Wibble();' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;

  ELSE_BEGIN_TEXT_WITH_RETURN = UNIT_HEADER +
    'procedure foo;' + AnsiLineBreak +
    'begin' + AnsiLineBreak +
    'if bar then // comment' + AnsiLineBreak +
    ' Fish()' + AnsiLineBreak +
    'else'+ AnsiLineBreak +
    'begin ' + AnsiLineBreak +
    '   Wibble();' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    'end;' + AnsiLineBreak +
    UNIT_FOOTER;


procedure TTestIfElseBreaks.TestAddElseBegin1;
begin
  FormatSettings.Returns.ElseBeginStyle := eAlways;
  TestProcessResult(TBlockStyles, ELSE_BEGIN_TEXT_WITH_RETURN, ELSE_BEGIN_TEXT_WITH_RETURN);
end;

procedure TTestIfElseBreaks.TestAddElseBegin2;
begin
  FormatSettings.Returns.ElseBeginStyle := eAlways;
  TestProcessResult(TBlockStyles, ELSE_BEGIN_TEXT_NO_RETURN, ELSE_BEGIN_TEXT_WITH_RETURN);
end;

procedure TTestIfElseBreaks.TestRemoveElseBegin1;
begin
  FormatSettings.Returns.ElseBeginStyle := eNever;
  TestProcessResult(TBlockStyles, ELSE_BEGIN_TEXT_WITH_RETURN, ELSE_BEGIN_TEXT_NO_RETURN);
end;

procedure TTestIfElseBreaks.TestRemoveElseBegin2;
begin
  FormatSettings.Returns.ElseBeginStyle := eNever;
  TestProcessResult(TBlockStyles, ELSE_BEGIN_TEXT_NO_RETURN, ELSE_BEGIN_TEXT_NO_RETURN);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestIfElseBreaks.Suite);
end.
