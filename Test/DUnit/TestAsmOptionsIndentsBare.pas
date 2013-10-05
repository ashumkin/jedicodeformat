unit TestAsmOptionsIndentsBare;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAsmOptionsIndentsBare, released October 2007.
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

{ test asm options for indents
  "Bare" in the sense that the asm block is not inside a proc,
   it is the top-level block of the proc
  This is JCF's prefered style
}

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform, IntList;

type

  TTestAsmOptionsIndentsBare = class(TBaseTestProcess)
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
    'procedure foo(i: integer);' + NativeLineBreak;

  UNIT_FOOTER = NativeLineBreak + NativeLineBreak +
    'end.';

  ASM_STATEMENTS_NOT_INDENTED =
    UNIT_HEADER +
    'asm' + NativeLineBreak +
    '  MOV ECX, [EDX]' + NativeLineBreak +
    '  XCHG ECX, [EAX]' + NativeLineBreak +
    '  CALL PROCASM2' + NativeLineBreak +
    'end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_INDENTED =
    UNIT_HEADER +
    'asm' + NativeLineBreak +
    '         MOV ECX, [EDX]' + NativeLineBreak +
    '         XCHG ECX, [EAX]' + NativeLineBreak +
    '         CALL PROCASM2' + NativeLineBreak +
    'end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_PARAMS_INDENTED =
    UNIT_HEADER +
    'asm' + NativeLineBreak +
    '         MOV     ECX, [EDX]' + NativeLineBreak +
    '         XCHG    ECX, [EAX]' + NativeLineBreak +
    '         CALL    PROCASM2' + NativeLineBreak +
    'end;' + NativeLineBreak +
    UNIT_FOOTER;


procedure TTestAsmOptionsIndentsBare.SetUp;
begin
  inherited;

  // store old settings
  with JcfFormatSettings do
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

procedure TTestAsmOptionsIndentsBare.TearDown;
begin
  inherited;

  with JcfFormatSettings do
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

procedure TTestAsmOptionsIndentsBare.TestStatementIndentOn;
begin
 JcfFormatSettings.SetAsm.StatementIndentEnabled := True;
 JcfFormatSettings.SetAsm.ParamsIndentEnabled := False;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_INDENTED);
end;


procedure TTestAsmOptionsIndentsBare.TestParamsIndentOn;
begin
 JcfFormatSettings.SetAsm.StatementIndentEnabled := True;
 JcfFormatSettings.SetAsm.ParamsIndentEnabled := True;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_PARAMS_INDENTED);
end;

procedure TTestAsmOptionsIndentsBare.TestStatementIndentOff;
begin
 JcfFormatSettings.SetAsm.StatementIndentEnabled := False;
 JcfFormatSettings.SetAsm.ParamsIndentEnabled := False;
 JcfFormatSettings.SetAsm.StatementIndent := 7;
  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_NOT_INDENTED);
end;


initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsIndentsBare.Suite);

end.
