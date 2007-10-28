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
------------------------------------------------------------------------------*)
{*)}

interface

{ test asm options for line breaking }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAsmOptionsBreaks = class(TBaseTestProcess)
  private
    fbBreaksAfterLabelEnabled: boolean;
    fbIndentsEnabled: boolean;
    feCapitalisation: TCapitalisationType;
    fiBreaksAfterLabel: integer;

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
  JclStrings,
  JCFSettings, SetAsm;


const
  UNIT_HEADER = 'unit CaseTest;' + AnsiLineBreak + AnsiLineBreak +
    'interface' + AnsiLineBreak + AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'uses Dialogs;' + AnsiLineBreak + AnsiLineBreak +
    'procedure foo(i: integer);' + AnsiLineBreak +
    'begin' + AnsiLineBreak;

  UNIT_FOOTER = AnsiLineBreak + 'end;' + AnsiLineBreak + AnsiLineBreak +
    'end.';

  ASM_LABEL_NONE =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    @@testLabel:    MOV   ECX, [EDX]' + AnsiLineBreak +
    '    XCHG  ECX, [EAX]' + AnsiLineBreak +
    '    CALL    PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

  ASM_LABEL_ONE =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    @@testLabel:' + AnsiLineBreak +
    '    MOV   ECX, [EDX]' + AnsiLineBreak +
    '    XCHG  ECX, [EAX]' + AnsiLineBreak +
    '    CALL    PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

    ASM_LABEL_TWO =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    @@testLabel:' + AnsiLineBreak +
    AnsiLineBreak +
    '    MOV   ECX, [EDX]' + AnsiLineBreak +
    '    XCHG  ECX, [EAX]' + AnsiLineBreak +
    '    CALL    PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

    ASM_LABEL_THREE =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    @@testLabel:' + AnsiLineBreak +
    AnsiLineBreak +
    AnsiLineBreak +
    '    MOV   ECX, [EDX]' + AnsiLineBreak +
    '    XCHG  ECX, [EAX]' + AnsiLineBreak +
    '    CALL    PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;




procedure TTestAsmOptionsBreaks.SetUp;
begin
  inherited;

  // store old settings
  with FormatSettings do
  begin
    fbBreaksAfterLabelEnabled := SetAsm.BreaksAfterLabelEnabled;
    fbIndentsEnabled := SetAsm.IndentsEnabled;
    feCapitalisation := SetAsm.Capitalisation;
    fiBreaksAfterLabel := SetAsm.BreaksAfterLabel;
  end;

end;

procedure TTestAsmOptionsBreaks.TearDown;
begin
  inherited;

  with FormatSettings do
  begin
    SetAsm.BreaksAfterLabelEnabled := fbBreaksAfterLabelEnabled;
    SetAsm.IndentsEnabled := fbIndentsEnabled;
    SetAsm.Capitalisation := feCapitalisation;
    SetAsm.BreaksAfterLabel := fiBreaksAfterLabel;
  end;
end;


procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_None;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_One;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_Two;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksNone_Three;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_NONE, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_None;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_One;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_Two;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksOne_Three;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_ONE, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_None;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_One;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_Two;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksTwo_Three;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_TWO, ASM_LABEL_THREE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_None;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 0;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_NONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_One;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 1;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_ONE);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_Two;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 2;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_TWO);
end;

procedure TTestAsmOptionsBreaks.TestLabelBreaksThree_Three;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 3;
  TestFormatResult(ASM_LABEL_THREE, ASM_LABEL_THREE);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsBreaks.Suite);
end.
