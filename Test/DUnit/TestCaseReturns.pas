unit TestCaseReturns;


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestSpacing, released May 2003.
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


{ AFS 10 Dec 2003
  Test block styles and returns in cases
}

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes;

type
  TTestCaseReturns = class(TBaseTestProcess)
  private
    feSaveCaseElseStyle: TTriOptionStyle;
    feCaseLabelStyle: TTriOptionStyle;

  protected
    procedure Setup; override;
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
  end;

implementation

uses JclStrings,
  JcfSettings,
  BlockStyles;

{ TTestCaseReturns }
const
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
    ' 1:' + AnsiLineBreak +
    'Bar; ' +
    ' 2:' + AnsiLineBreak +
    'Fish; ' +
    ' else' + AnsiLineBreak +
    'Spock; ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

  OUT_UNIT_TEXT_JUST_ELSE_NEWLINE = UNIT_HEADER + ' procedure foo; begin' +
    ' case x of ' +
    ' 1: Bar; ' +
    ' 2: Fish; ' +
    ' else' + AnsiLineBreak +
    'Spock; ' +
    ' end; ' +
    ' end; ' +
    UNIT_FOOTER;

procedure TTestCaseReturns.Setup;
begin
  inherited;

  feSaveCaseElseStyle := FormatSettings.Returns.CaseElseStyle;
  feCaseLabelStyle    := FormatSettings.Returns.CaseLabelStyle;
end;

procedure TTestCaseReturns.TearDown;
begin
  inherited;

  FormatSettings.Returns.CaseElseStyle  := feSaveCaseElseStyle;
  FormatSettings.Returns.CaseLabelStyle := feCaseLabelStyle;
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

initialization
  TestFramework.RegisterTest('Processes', TTestCaseReturns.Suite);
end.
