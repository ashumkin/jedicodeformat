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
------------------------------------------------------------------------------*)
{*)}

interface

uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestRemoveBlankLines = class(TBaseTestProcess)
  private
  public
    procedure Setup; override;
  published
    procedure TestNone;
    procedure Test1;
  end;

implementation

uses
  JclStrings,
  JcfSettings, RemoveConsecutiveReturns;

procedure TTestRemoveBlankLines.Setup;
begin
  inherited;
  FormatSettings.Returns.MaxConsecutiveBlankLines := 4;
  FormatSettings.Returns.RemoveConsecutiveBlankLines := True;
end;

procedure TTestRemoveBlankLines.TestNone;
const
  IN_UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveConsecutiveReturns, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestRemoveBlankLines.Test1;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER +
    AnsiLineBreak + AnsiLineBreak +
    AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TRemoveConsecutiveReturns, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestRemoveBlankLines.Suite);
end.
