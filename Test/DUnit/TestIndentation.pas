unit TestIndentation;

{ AFS Jan 2003
  Test the indentation process }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestSpacing, released January 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2004 Anthony Steele.
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
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess;

type
  TTestIndentation = class(TBaseTestProcess)
  private
    fbSaveIndentBeginEnd: boolean;
    fiSaveIndentSpaces, fiSaveIndentBeginEndSpaces: integer;
    fbSaveHasFirstLevelIndent: boolean;
    fiSaveFirstLevelIndent: integer;

  protected
    procedure Setup; override;
    procedure Teardown; override;
  published


    procedure TestIndent1Space;
    procedure TestIndent2Spaces;
    procedure TestIndent3Spaces;

    procedure TestFirstLevelIndent1;
    procedure TestFirstLevelIndent2;
    procedure TestFirstLevelIndent3;

    procedure TestIndenterBeginEnd1;
    procedure TestIndenterBeginEnd2;
    procedure TestIndenterBeginEnd3;
  end;

implementation

uses JclStrings,
  JcfSettings,
  Indenter, SetIndent;

procedure TTestIndentation.Setup;
begin
  inherited;

  // save settings
  fbSaveIndentBeginEnd := FormatSettings.Indent.IndentBeginEnd;
  fiSaveIndentSpaces := FormatSettings.Indent.IndentSpaces;
  fiSaveIndentBeginEndSpaces := FormatSettings.Indent.IndentBeginEndSpaces;

  fbSaveHasFirstLevelIndent := FormatSettings.Indent.HasFirstLevelIndent;
  fiSaveFirstLevelIndent := FormatSettings.Indent.FirstLevelIndent;
end;

procedure TTestIndentation.Teardown;
begin
  inherited;

  // restore the settings
  FormatSettings.Indent.IndentBeginEnd := fbSaveIndentBeginEnd;
  FormatSettings.Indent.IndentSpaces := fiSaveIndentSpaces;
  FormatSettings.Indent.IndentBeginEndSpaces := fiSaveIndentBeginEndSpaces;

  FormatSettings.Indent.HasFirstLevelIndent := fbSaveHasFirstLevelIndent;
  FormatSettings.Indent.FirstLevelIndent := fiSaveFirstLevelIndent;
end;

const
  BASIC_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     'result := 2 + 2;' + AnsiLineBreak +
     'end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;

     { the various out unit texts differ only in how much white space there is
       before the lines of code in procedure foo. }


procedure TTestIndentation.TestIndent1Space;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     ' begin' + AnsiLineBreak +
     '  result := 2 + 2;' + AnsiLineBreak +
     ' end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 1;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestIndent2Spaces;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '  begin' + AnsiLineBreak +
     '    result := 2 + 2;' + AnsiLineBreak +
     '  end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestIndent3Spaces;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '   begin' + AnsiLineBreak +
     '      result := 2 + 2;' + AnsiLineBreak +
     '   end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 3;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestFirstLevelIndent1;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     ' begin' + AnsiLineBreak +
     '   result := 2 + 2;' + AnsiLineBreak +
     ' end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.HasFirstLevelIndent := True;
  FormatSettings.Indent.FirstLevelIndent := 1;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestFirstLevelIndent2;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '  begin' + AnsiLineBreak +
     '    result := 2 + 2;' + AnsiLineBreak +
     '  end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.HasFirstLevelIndent := True;
  FormatSettings.Indent.FirstLevelIndent := 2;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestFirstLevelIndent3;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '   begin' + AnsiLineBreak +
     '     result := 2 + 2;' + AnsiLineBreak +
     '   end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := False;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.HasFirstLevelIndent := True;
  FormatSettings.Indent.FirstLevelIndent := 3;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestIndenterBeginEnd1;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '   begin' + AnsiLineBreak +
     '    result := 2 + 2;' + AnsiLineBreak +
     '   end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := True;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.IndentBeginEndSpaces := 1;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestIndenterBeginEnd2;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '    begin' + AnsiLineBreak +
     '    result := 2 + 2;' + AnsiLineBreak +
     '    end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := True;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.IndentBeginEndSpaces := 2;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestIndentation.TestIndenterBeginEnd3;
const
  OUT_UNIT_TEXT = UNIT_HEADER +
     'function foo: integer;' + AnsiLineBreak +
     'begin' + AnsiLineBreak +
     '     begin' + AnsiLineBreak +
     '    result := 2 + 2;' + AnsiLineBreak +
     '     end' + AnsiLineBreak +
     'end;' + UNIT_FOOTER;
begin
  FormatSettings.Indent.IndentBeginEnd := True;
  FormatSettings.Indent.IndentSpaces := 2;
  FormatSettings.Indent.IndentBeginEndSpaces := 3;
  FormatSettings.Indent.HasFirstLevelIndent := False;

  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestIndentation.Suite);
end.
