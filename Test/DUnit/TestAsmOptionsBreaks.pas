unit TestAsmOptionsBreaks;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestAsmOptionsBreaks, released October 2007.
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

{ test asm options for line breaking }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAsmOptionsBreaks = class(TBaseTestProcess)
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

    procedure TestLabelBreaksNone_None;
    procedure TestLabelBreaksNone_One;
    procedure TestLabelBreaksNone_Three;
    procedure TestLabelBreaksNone_Two;

    procedure TestLabelBreaksOne_None;
    procedure TestLabelBreaksOne_One;
    procedure TestLabelBreaksOne_Two;
    procedure TestLabelBreaksOne_Three;

    procedure TestLabelBreaksTwo_None;
    procedure TestLabelBreaksTwo_One;
    procedure TestLabelBreaksTwo_Two;
    procedure TestLabelBreaksTwo_Three;

    procedure TestLabelBreaksThree_None;
    procedure TestLabelBreaksThree_One;
    procedure TestLabelBreaksThree_Two;
    procedure TestLabelBreaksThree_Three;
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

  ASM_LABEL_NONE =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    @@testLabel:    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

  ASM_LABEL_ONE =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    @@testLabel:' + NativeLineBreak +
    '    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_LABEL_TWO =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    @@testLabel:' + NativeLineBreak +
    NativeLineBreak +
    '    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;

    ASM_LABEL_THREE =
    UNIT_HEADER +
    '  asm' + NativeLineBreak +
    '    @@testLabel:' + NativeLineBreak +
    NativeLineBreak +
    NativeLineBreak +
    '    MOV   ECX, [EDX]' + NativeLineBreak +
    '    XCHG  ECX, [EAX]' + NativeLineBreak +
    '    CALL    PROCASM2' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    UNIT_FOOTER;




procedure TTestAsmOptionsBreaks.SetUp;
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
    SetAsm.BreaksAfterLabelEnabled := True;
  end;

end;

procedure TTestAsmOptionsBreaks.TearDown;
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


procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_None;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_One;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_Two;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_Three;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_None;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_One;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_Two;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_Three;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_None;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_One;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_Two;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_Three;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_None;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_One;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_Two;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_Three;
begin
 JcfFormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_THREE);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsBreaks.Suite);
end.
