unit TestIndentElse;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestIndentElse, released April 2006.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2006 Anthony Steele.
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


{ AFS April 2006
  Test the indentation of else cases }


uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestIndentElse = class(TBaseTestProcess)
  private
    fbSaveIndentBeginEnd: boolean;
    fiSaveIndentSpaces, fiSaveIndentBeginEndSpaces: integer;
    fbSaveHasFirstLevelIndent: boolean;
    fiSaveFirstLevelIndent: integer;
    fbSaveIndentElse: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestBasic;
    procedure TestIfElse1;
    procedure TestIfElse2;
    procedure TestIfElse3;
    procedure TestIfElseStatements;
  end;

implementation

uses
  JcfStringUtils,
  JcfSettings,
  Indenter;

procedure TTestIndentElse.Setup;
begin
  inherited;

  // save settings
  fbSaveIndentBeginEnd := JcfFormatSettings.Indent.IndentBeginEnd;
  fiSaveIndentSpaces   := JcfFormatSettings.Indent.IndentSpaces;
  fiSaveIndentBeginEndSpaces := JcfFormatSettings.Indent.IndentBeginEndSpaces;

  fbSaveHasFirstLevelIndent := JcfFormatSettings.Indent.HasFirstLevelIndent;
  fiSaveFirstLevelIndent    := JcfFormatSettings.Indent.FirstLevelIndent;
  fbSaveIndentElse := JcfFormatSettings.Indent.IndentElse;

 JcfFormatSettings.Indent.HasFirstLevelIndent := False;
 JcfFormatSettings.Indent.IndentBeginEnd := False;
 JcfFormatSettings.Indent.IndentSpaces := 2;
 JcfFormatSettings.Indent.IndentElse := True;
end;

procedure TTestIndentElse.Teardown;
begin
  inherited;

  // restore the settings
 JcfFormatSettings.Indent.IndentBeginEnd := fbSaveIndentBeginEnd;
 JcfFormatSettings.Indent.IndentSpaces   := fiSaveIndentSpaces;
 JcfFormatSettings.Indent.IndentBeginEndSpaces := fiSaveIndentBeginEndSpaces;

 JcfFormatSettings.Indent.HasFirstLevelIndent := fbSaveHasFirstLevelIndent;
 JcfFormatSettings.Indent.FirstLevelIndent    := fiSaveFirstLevelIndent;

 JcfFormatSettings.Indent.IndentElse := fbSaveIndentElse;
end;

const
  BASIC_UNIT_TEXT = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;


procedure TTestIndentElse.TestBasic;
begin
  TestProcessResult(TIndenter, BASIC_UNIT_TEXT, BASIC_UNIT_TEXT);
end;

const
  IF_ELSE_1_IN = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > c then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

  IF_ELSE_1_OUT = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    if a > c then' + NativeLineBreak +
    '    begin' + NativeLineBreak +
    '    end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

procedure TTestIndentElse.TestIfElse1;
begin
  TestProcessResult(TIndenter, IF_ELSE_1_IN, IF_ELSE_1_OUT);
end;

const
  IF_ELSE_2_IN = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > c then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end'+ NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > d then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

  IF_ELSE_2_OUT = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    if a > c then' + NativeLineBreak +
    '    begin' + NativeLineBreak +
    '    end'+ NativeLineBreak +
    '    else' + NativeLineBreak +
    '      if a > d then' + NativeLineBreak +
    '      begin' + NativeLineBreak +
    '      end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

procedure TTestIndentElse.TestIfElse2;
begin
  TestProcessResult(TIndenter, IF_ELSE_2_IN, IF_ELSE_2_OUT);
end;

const
  IF_ELSE_3_IN = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > c then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end'+ NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > d then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end'+ NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > e then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

  IF_ELSE_3_OUT = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    if a > c then' + NativeLineBreak +
    '    begin' + NativeLineBreak +
    '    end'+ NativeLineBreak +
    '    else' + NativeLineBreak +
    '      if a > d then' + NativeLineBreak +
    '      begin' + NativeLineBreak +
    '      end'+ NativeLineBreak +
    '      else' + NativeLineBreak +
    '        if a > e then' + NativeLineBreak +
    '        begin' + NativeLineBreak +
    '        end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;


procedure TTestIndentElse.TestIfElse3;
begin
  TestProcessResult(TIndenter, IF_ELSE_3_IN, IF_ELSE_3_OUT);
end;

const
  IF_ELSE_STATEMENTS = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    a := b;' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > c then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    a := b;' + NativeLineBreak +
    '  end'+ NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > d then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    a := b;' + NativeLineBreak +
    '  end'+ NativeLineBreak +
    '  else' + NativeLineBreak +
    '  if a > e then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    a := b;' + NativeLineBreak +
    '  end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

  IF_ELSE_STATEMENTS_OUT = UNIT_HEADER +
    'function foo: integer;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  if a > b then' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    a := b;' + NativeLineBreak +
    '  end' + NativeLineBreak +
    '  else' + NativeLineBreak +
    '    if a > c then' + NativeLineBreak +
    '    begin' + NativeLineBreak +
    '      a := b;' + NativeLineBreak +
    '    end'+ NativeLineBreak +
    '    else' + NativeLineBreak +
    '      if a > d then' + NativeLineBreak +
    '      begin' + NativeLineBreak +
    '        a := b;' + NativeLineBreak +
    '      end'+ NativeLineBreak +
    '      else' + NativeLineBreak +
    '        if a > e then' + NativeLineBreak +
    '        begin' + NativeLineBreak +
    '          a := b;' + NativeLineBreak +
    '        end;'+ NativeLineBreak +
    'end;' + UNIT_FOOTER;

procedure TTestIndentElse.TestIfElseStatements;
begin
  TestProcessResult(TIndenter, IF_ELSE_STATEMENTS, IF_ELSE_STATEMENTS_OUT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestIndentElse.Suite);
end.
