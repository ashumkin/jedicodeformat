unit TestAsmOptionsIndents;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAsmOptionsIndents, released October 2007.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
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

{ test asm options for indents }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform, IntList;

type

  TTestAsmOptionsIndents = class(TBaseTestProcess)
  private
    feCapitalisation: TCapitalisationType;

    fbBreaksAfterLabelEnabled: boolean;
    fiBreaksAfterLabel: integer;

    fbStatementIndentEnabled: boolean;
    fiStatementIndent: integer;
    fbParamsIndentEnabled: boolean;
    fiParamsIndent: integer;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public

  published
    procedure TestParamsIndentOn;
    procedure TestStatementIndentOn;
    procedure TestStatementIndentOff;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils,
  JCFSettings, SetAsm;

const
  UNIT_HEADER = 'unit CaseTest;' + NativeLineBreak + NativeLineBreak +
    'interface' + NativeLineBreak + NativeLineBreak +
    'implementation' + NativeLineBreak + NativeLineBreak +
    'uses Dialogs;' + NativeLineBreak + NativeLineBreak +
    'procedure foo(i: integer);' + NativeLineBreak +
    'begin' + NativeLineBreak;

  UNIT_FOOTER = NativeLineBreak + 'end;' + NativeLineBreak + NativeLineBreak +
    'end.';

  ASM_STATEMENTS_NOT_INDENTED =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    MOV ECX, [EDX]' + NativeLineBreak +
    '    XCHG ECX, [EAX]' + NativeLineBreak +
    '    CALL PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_INDENTED =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '           MOV ECX, [EDX]' + NativeLineBreak +
    '           XCHG ECX, [EAX]' + NativeLineBreak +
    '           CALL PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_PARAMS_INDENTED =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '           MOV     ECX, [EDX]' + NativeLineBreak +
    '           XCHG    ECX, [EAX]' + NativeLineBreak +
    '           CALL    PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;


procedure TTestAsmOptionsIndents.SetUp;
begin
  inherited;

  // store old settings
  with FormatSettings do
  begin
    feCapitalisation := SetAsm.Capitalisation;

    fbBreaksAfterLabelEnabled := SetAsm.BreaksAfterLabelEnabled;
    fiBreaksAfterLabel := SetAsm.BreaksAfterLabel;

    fbStatementIndentEnabled := SetAsm.StatementIndentEnabled;
    fiStatementIndent := SetAsm.StatementIndent;

    fbParamsIndentEnabled := SetAsm.ParamsIndentEnabled;
    fiParamsIndent := SetAsm.ParamsIndent;
  end;
end;

procedure TTestAsmOptionsIndents.TearDown;
begin
  inherited;

  with FormatSettings do
  begin
    SetAsm.Capitalisation := feCapitalisation;

    SetAsm.BreaksAfterLabelEnabled := fbBreaksAfterLabelEnabled;
    SetAsm.BreaksAfterLabel := fiBreaksAfterLabel;

    SetAsm.StatementIndentEnabled := fbStatementIndentEnabled;
    SetAsm.StatementIndent := fiStatementIndent;

    SetAsm.ParamsIndentEnabled := fbParamsIndentEnabled;
    SetAsm.ParamsIndent := fiParamsIndent;
  end;

end;

procedure TTestAsmOptionsIndents.TestStatementIndentOn;
begin
  FormatSettings.SetAsm.StatementIndentEnabled := True;
  FormatSettings.SetAsm.ParamsIndentEnabled := False;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_INDENTED);
end;


procedure TTestAsmOptionsIndents.TestParamsIndentOn;
begin
  FormatSettings.SetAsm.StatementIndentEnabled := True;
  FormatSettings.SetAsm.ParamsIndentEnabled := True;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_PARAMS_INDENTED);
end;

procedure TTestAsmOptionsIndents.TestStatementIndentOff;
begin
  FormatSettings.SetAsm.StatementIndentEnabled := False;
  FormatSettings.SetAsm.ParamsIndentEnabled := False;
  FormatSettings.SetAsm.StatementIndent := 7;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_NOT_INDENTED);
end;


initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsIndents.Suite);

end.
