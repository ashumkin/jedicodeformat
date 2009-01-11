unit TestMaxBlankLines;

{ AFS 10 Nov 2003
  Test blank lines removal }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestMaxBlankLines, released November 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
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
  BaseTestProcess;

type
  TTestRemoveBlankLines = class(TBaseTestProcess)
  private
    fiMaxLines, fiMaxLinesInSection: integer;
    fbRemoveCosecutiveBlankLines: Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNone;
    procedure Test1;

    procedure TestVarLines0;
    procedure TestVarLines1;
    procedure TestVarLines2;
    procedure TestVarLines3;
    procedure TestVarLines4;

    procedure TestProcHeaderLines0;
    procedure TestProcHeaderLines1;
    procedure TestProcHeaderLines2;
    procedure TestProcHeaderLines3;
    procedure TestProcHeaderLines4;
  end;

implementation

uses
  JcfStringUtils,
  JcfSettings, SetReturns,
  RemoveConsecutiveReturns, RemoveBlankLinesInVars, RemoveBlankLinesAfterProcHeader;

procedure TTestRemoveBlankLines.Setup;
begin
  inherited;
  fiMaxLines := FormatSettings.Returns.MaxConsecutiveBlankLines;
  fiMaxLinesInSection := FormatSettings.Returns.MaxBlankLinesInSection;
  fbRemoveCosecutiveBlankLines := FormatSettings.Returns.RemoveConsecutiveBlankLines;

  FormatSettings.Returns.MaxConsecutiveBlankLines    := 4;
  FormatSettings.Returns.RemoveConsecutiveBlankLines := True;
end;


procedure TTestRemoveBlankLines.TearDown;
begin
  inherited;

  // restore initial values
  FormatSettings.Returns.MaxConsecutiveBlankLines := fiMaxLines;
  FormatSettings.Returns.MaxBlankLinesInSection := fiMaxLinesInSection;
  FormatSettings.Returns.RemoveConsecutiveBlankLines := fbRemoveCosecutiveBlankLines;
end;

procedure TTestRemoveBlankLines.TestNone;
const
  IN_UNIT_TEXT  = UNIT_HEADER + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveConsecutiveReturns, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestRemoveBlankLines.Test1;
const
  IN_UNIT_TEXT  = UNIT_HEADER +
    NativeLineBreak + NativeLineBreak +
    NativeLineBreak + NativeLineBreak +
    NativeLineBreak + NativeLineBreak +
    NativeLineBreak + NativeLineBreak +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER +
    NativeLineBreak + NativeLineBreak +
    NativeLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TRemoveConsecutiveReturns, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


const
  UNIT_TEXT_VAR_LINES_0  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_VAR_LINES_1  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_VAR_LINES_2  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    NativeLineBreak +
    NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;


  UNIT_TEXT_VAR_LINES_3  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    NativeLineBreak +
    NativeLineBreak +
    NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;


procedure TTestRemoveBlankLines.TestVarLines0;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 0;
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_0, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_1, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_2, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_3, UNIT_TEXT_VAR_LINES_0);
end;

procedure TTestRemoveBlankLines.TestVarLines1;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 1;
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_0, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_1, UNIT_TEXT_VAR_LINES_1);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_2, UNIT_TEXT_VAR_LINES_1);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_3, UNIT_TEXT_VAR_LINES_1);
end;

procedure TTestRemoveBlankLines.TestVarLines2;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 2;
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_0, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_1, UNIT_TEXT_VAR_LINES_1);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_2, UNIT_TEXT_VAR_LINES_2);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_3, UNIT_TEXT_VAR_LINES_2);
end;

procedure TTestRemoveBlankLines.TestVarLines3;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 3;
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_0, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_1, UNIT_TEXT_VAR_LINES_1);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_2, UNIT_TEXT_VAR_LINES_2);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_3, UNIT_TEXT_VAR_LINES_3);
end;

procedure TTestRemoveBlankLines.TestVarLines4;
begin
  // as per 3
  FormatSettings.Returns.MaxBlankLinesInSection := 4;
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_0, UNIT_TEXT_VAR_LINES_0);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_1, UNIT_TEXT_VAR_LINES_1);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_2, UNIT_TEXT_VAR_LINES_2);
  TestProcessResult(TRemoveBlankLinesInVars, UNIT_TEXT_VAR_LINES_3, UNIT_TEXT_VAR_LINES_3);
end;

const
  UNIT_TEXT_HEADER_LINES_0  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_HEADER_LINES_1  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
     NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_HEADER_LINES_2  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
     NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_TEXT_HEADER_LINES_3  = UNIT_HEADER +
    ' procedure foo; ' + NativeLineBreak +
     NativeLineBreak +
    'var' + NativeLineBreak +
    '  a: integer;' + NativeLineBreak +
    'begin end;' + NativeLineBreak +
    UNIT_FOOTER;



procedure TTestRemoveBlankLines.TestProcHeaderLines0;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 0;
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_0, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_1, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_2, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_3, UNIT_TEXT_HEADER_LINES_0);
end;

procedure TTestRemoveBlankLines.TestProcHeaderLines1;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 1;
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_0, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_1, UNIT_TEXT_HEADER_LINES_1);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_2, UNIT_TEXT_HEADER_LINES_1);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_3, UNIT_TEXT_HEADER_LINES_1);
end;

procedure TTestRemoveBlankLines.TestProcHeaderLines2;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 2;
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_0, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_1, UNIT_TEXT_HEADER_LINES_1);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_2, UNIT_TEXT_HEADER_LINES_2);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_3, UNIT_TEXT_HEADER_LINES_2);
end;

procedure TTestRemoveBlankLines.TestProcHeaderLines3;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 3;
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_0, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_1, UNIT_TEXT_HEADER_LINES_1);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_2, UNIT_TEXT_HEADER_LINES_2);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_3, UNIT_TEXT_HEADER_LINES_3);
end;

procedure TTestRemoveBlankLines.TestProcHeaderLines4;
begin
  FormatSettings.Returns.MaxBlankLinesInSection := 4;
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_0, UNIT_TEXT_HEADER_LINES_0);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_1, UNIT_TEXT_HEADER_LINES_1);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_2, UNIT_TEXT_HEADER_LINES_2);
  TestProcessResult(TRemoveBlankLinesAfterProcHeader, UNIT_TEXT_HEADER_LINES_3, UNIT_TEXT_HEADER_LINES_3);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestRemoveBlankLines.Suite);
end.
