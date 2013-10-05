unit TestAsmOptionsCaps;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAsmOptionsCaps, released October 2007.
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

{ test asm options for capitalisation }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAsmOptionsCaps = class(TBaseTestProcess)
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

  published
    procedure TestCaps_LowerToUpper;
    procedure TestCaps_MixedToUpper;
    procedure TestCaps_UpperToUpper;

    procedure TestCaps_LowerToMixed;
    procedure TestCaps_MixedToMixed;
    procedure TestCaps_UpperToMixed;

    procedure TestCaps_LowerToLower;
    procedure TestCaps_MixedToLower;
    procedure TestCaps_UpperToLower;

    procedure TestCaps_LowerLeaveAlone;
    procedure TestCaps_MixedLeaveAlone;
    procedure TestCaps_UpperLeaveAlone;

    procedure TestCaps_VarToCaps;
  end;

implementation

uses
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

  ASM_STATEMENTS_UPPER =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    Procasm2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_LOWER =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    mov   ecx, [edx]' + NativeLineBreak +
    '    xchg  ecx, [eax]' + NativeLineBreak +
    '    call    Procasm2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;


    ASM_STATEMENTS_MIXED =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    MOV   ecx, [EDX]' + NativeLineBreak +
    '    xchg  ECX, [eax]' + NativeLineBreak +
    '    CALL    Procasm2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_INITALCAPS =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    Mov   Ecx, [Edx]' + NativeLineBreak +
    '    Xchg  Ecx, [Eax]' + NativeLineBreak +
    '    Call    Procasm2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;


    ASM_STATEMENTS_VarCaps_Lower =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    Mov   Ecx, [Edx]' + NativeLineBreak +
    '    Xchg  Ecx, [Eax]' + NativeLineBreak +
    '    Call    Procasm2' + NativeLineBreak +
    '    jmp   [_NetGroupDel]' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_VarCaps_Upper =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    Procasm2' + NativeLineBreak +
    '    JMP   [_NetGroupDel]' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;


procedure TTestAsmOptionsCaps.SetUp;
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

    SetAsm.StatementIndentEnabled := False;
    SetAsm.ParamsIndentEnabled := False;
  end;
end;

procedure TTestAsmOptionsCaps.TearDown;
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


procedure TTestAsmOptionsCaps.TestCaps_UpperLeaveAlone;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToLower;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToMixed;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToUpper;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_UPPER);
end;


procedure TTestAsmOptionsCaps.TestCaps_MixedLeaveAlone;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_MIXED);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToLower;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToMixed;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToUpper;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerLeaveAlone;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToLower;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToMixed;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToUpper;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_VarToCaps;
begin
 JcfFormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_VarCaps_Lower, ASM_STATEMENTS_VarCaps_Upper);

end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsCaps.Suite);
end.
