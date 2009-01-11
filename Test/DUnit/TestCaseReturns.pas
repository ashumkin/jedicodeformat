unit TestCaseReturns;


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestCaseReturns, released May 2003.
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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface


{ AFS 10 Dec 2003
  Test block styles and returns in cases
}

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes;

type
  TTestCaseReturns = class(TBaseTestProcess)
  private
    feSaveCaseLabelStyle: TTriOptionStyle;
    feSaveCaseBeginStyle: TTriOptionStyle;
    feSaveCaseElseStyle: TTriOptionStyle;
    feSaveCaseElseBeginStyle:  TTriOptionStyle;
    fbSaveCaseElseIndent: boolean;


  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestCaseStatementLeave1;
    procedure TestCaseStatementLeave2;
    procedure TestCaseStatementLeave3;

    procedure TestCaseStatementNever1;
    procedure TestCaseStatementNever2;
    procedure TestCaseStatementNever3;

    procedure TestCaseStatementAlways1;
    procedure TestCaseStatementAlways2;
    procedure TestCaseStatementAlways3;

    procedure TestCaseStatementJustElse1;
    procedure TestCaseStatementJustElse2;
    procedure TestCaseStatementJustElse3;

    procedure TestCaseStatementElseBegin1;
    procedure TestCaseStatementElseBegin2;
    procedure TestCaseStatementElseBegin3;
    procedure TestCaseStatementElseBegin4;
    procedure TestCaseStatementElseBegin5;
    procedure TestCaseStatementElseBegin6;

    procedure TestCaseBegin1;
    procedure TestCaseBegin2;
    procedure TestCaseBegin3;
    procedure TestCaseBegin4;
    procedure TestCaseBegin5;
    procedure TestCaseBegin6;

    procedure TestCaseStatementElseIndent;
    procedure TestCaseStatementElseOutdent;
  end;

implementation

uses
  JcfStringUtils,
  JcfSettings,
  BlockStyles, Indenter, SetReturns;

{ TTestCaseReturns }
const

  { test breaking after case labels }
  UNIT_TEXT_IN_LINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1: Bar; ' +
    ' 2: Fish; ' +
    ' else Spock; ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

  UNIT_TEXT_NEW_LINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1:' + NativeLineBreak +
    'Bar; ' +
    ' 2:' + NativeLineBreak +
    'Fish; ' +
    ' else' + NativeLineBreak +
    'Spock; ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

  OUT_UNIT_TEXT_JUST_ELSE_NEWLINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1: Bar; ' +
    ' 2: Fish; ' +
    ' else' + NativeLineBreak +
    'Spock; ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;


  { test breaking on else ... begin}
  UNIT_TEXT_ELSE_BEGIN = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1:' + NativeLineBreak +
    'Bar; ' +
    ' 2:' + NativeLineBreak +
    'Fish; ' +
    ' else begin' + NativeLineBreak +
    'Spock; ' +
    ' end; end; ' +
    ' end; ' +
    UNIT_FOOTER;


  UNIT_TEXT_ELSE_BEGIN_NEWLINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1:' + NativeLineBreak +
    'Bar; ' +
    ' 2:' + NativeLineBreak +
    'Fish; ' +
    ' else' + NativeLineBreak +
    'begin' + NativeLineBreak +
    'Spock; ' +
    ' end; end; ' +
    ' end; ' +
    UNIT_FOOTER;

  { test breaking on case: begin }
  UNIT_TEXT_CASE_BEGIN = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1: begin Bar;  end ' +
    ' 2: begin Fish; end ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

  UNIT_TEXT_CASE_BEGIN_NEW_LINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1:' + NativeLineBreak +
    'begin Bar;  end ' +
    ' 2:' + NativeLineBreak +
    'begin Fish; end ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

  UNIT_TEXT_INDENTED = UNIT_HEADER + NativeLineBreak +
    'procedure foo;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  case x of' + NativeLineBreak +
    '    1: Bar;' + NativeLineBreak +
    '    2: Fish;' + NativeLineBreak +
    '    else Spock;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_INDENTED_ELSE = UNIT_HEADER + NativeLineBreak +
    'procedure foo;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  case x of' + NativeLineBreak +
    '    1: Bar;' + NativeLineBreak +
    '    2: Fish;' + NativeLineBreak +
    '  else Spock;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'end;' + NativeLineBreak +
    UNIT_FOOTER;

procedure TTestCaseReturns.Setup;
begin
  inherited;

  feSaveCaseLabelStyle := FormatSettings.Returns.CaseLabelStyle;
  feSaveCaseBeginStyle := FormatSettings.Returns.CaseBeginStyle;

  feSaveCaseElseStyle := FormatSettings.Returns.CaseElseStyle;
  feSaveCaseElseBeginStyle := FormatSettings.Returns.CaseElseBeginStyle;

  fbSaveCaseElseIndent := FormatSettings.Indent.IndentCaseElse;
end;

procedure TTestCaseReturns.TearDown;
begin
  inherited;

  FormatSettings.Returns.CaseLabelStyle := feSaveCaseLabelStyle;
  FormatSettings.Returns.CaseBeginStyle := feSaveCaseBeginStyle;

  FormatSettings.Returns.CaseElseStyle  := feSaveCaseElseStyle;
  FormatSettings.Returns.CaseElseBeginStyle := feSaveCaseElseBeginStyle;

  FormatSettings.Indent.IndentCaseElse := fbSaveCaseElseIndent;
end;


procedure TTestCaseReturns.TestCaseStatementAlways1;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eAlways;

  TestProcessResult(TBlockStyles, UNIT_TEXT_IN_LINE, UNIT_TEXT_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementAlways2;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eAlways;

  TestProcessResult(TBlockStyles, UNIT_TEXT_NEW_LINE, UNIT_TEXT_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementAlways3;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eAlways;

  TestProcessResult(TBlockStyles, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE, UNIT_TEXT_NEW_LINE);
end;



procedure TTestCaseReturns.TestCaseStatementJustElse1;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, UNIT_TEXT_IN_LINE, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementJustElse2;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, UNIT_TEXT_NEW_LINE, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementJustElse3;
begin
  FormatSettings.Returns.CaseElseStyle  := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE,
    OUT_UNIT_TEXT_JUST_ELSE_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementLeave1;
begin
  FormatSettings.Returns.CaseElseStyle  := eLeave;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_NEW_LINE, UNIT_TEXT_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementLeave2;
begin
  FormatSettings.Returns.CaseElseStyle  := eLeave;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_IN_LINE, UNIT_TEXT_IN_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementLeave3;
begin
  FormatSettings.Returns.CaseElseStyle  := eLeave;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE,
    OUT_UNIT_TEXT_JUST_ELSE_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementNever1;
begin
  FormatSettings.Returns.CaseElseStyle  := eNever;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, UNIT_TEXT_NEW_LINE, UNIT_TEXT_IN_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementNever2;
begin
  FormatSettings.Returns.CaseElseStyle  := eNever;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, OUT_UNIT_TEXT_JUST_ELSE_NEWLINE, UNIT_TEXT_IN_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementNever3;
begin
  FormatSettings.Returns.CaseElseStyle  := eNever;
  FormatSettings.Returns.CaseLabelStyle := eNever;

  TestProcessResult(TBlockStyles, UNIT_TEXT_NEW_LINE, UNIT_TEXT_IN_LINE);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin1;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eNever;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN, UNIT_TEXT_ELSE_BEGIN);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin2;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eNever;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN_NEWLINE, UNIT_TEXT_ELSE_BEGIN);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin3;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eLeave;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN, UNIT_TEXT_ELSE_BEGIN);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin4;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eLeave;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN_NEWLINE, UNIT_TEXT_ELSE_BEGIN_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin5;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eAlways;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN, UNIT_TEXT_ELSE_BEGIN_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseStatementElseBegin6;
begin
  FormatSettings.Returns.CaseElseStyle      := eNever;
  FormatSettings.Returns.CaseElseBeginStyle := eAlways;
  FormatSettings.Returns.CaseLabelStyle     := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_ELSE_BEGIN_NEWLINE, UNIT_TEXT_ELSE_BEGIN_NEWLINE);
end;

procedure TTestCaseReturns.TestCaseBegin1;
begin
  FormatSettings.Returns.CaseBeginStyle := eLeave;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN, UNIT_TEXT_CASE_BEGIN);
end;

procedure TTestCaseReturns.TestCaseBegin2;
begin
  FormatSettings.Returns.CaseBeginStyle := eLeave;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN_NEW_LINE, UNIT_TEXT_CASE_BEGIN_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseBegin3;
begin
  FormatSettings.Returns.CaseBeginStyle := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN, UNIT_TEXT_CASE_BEGIN_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseBegin4;
begin
  FormatSettings.Returns.CaseBeginStyle := eAlways;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN_NEW_LINE, UNIT_TEXT_CASE_BEGIN_NEW_LINE);
end;

procedure TTestCaseReturns.TestCaseBegin5;
begin
  FormatSettings.Returns.CaseBeginStyle := eNever;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN, UNIT_TEXT_CASE_BEGIN);
end;

procedure TTestCaseReturns.TestCaseBegin6;
begin
  FormatSettings.Returns.CaseBeginStyle := eNever;
  FormatSettings.Returns.CaseLabelStyle := eLeave;

  TestProcessResult(TBlockStyles, UNIT_TEXT_CASE_BEGIN_NEW_LINE, UNIT_TEXT_CASE_BEGIN);
end;


procedure TTestCaseReturns.TestCaseStatementElseIndent;
begin
  FormatSettings.Indent.IndentCaseElse := True;

  TestProcessResult(TIndenter, UNIT_TEXT_INDENTED, UNIT_TEXT_INDENTED);
  TestProcessResult(TIndenter, UNIT_TEXT_INDENTED_ELSE, UNIT_TEXT_INDENTED);
end;

procedure TTestCaseReturns.TestCaseStatementElseOutdent;
begin
  FormatSettings.Indent.IndentCaseElse := False;

  TestProcessResult(TIndenter, UNIT_TEXT_INDENTED, UNIT_TEXT_INDENTED_ELSE);
  TestProcessResult(TIndenter, UNIT_TEXT_INDENTED_ELSE, UNIT_TEXT_INDENTED_ELSE);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestCaseReturns.Suite);
end.
