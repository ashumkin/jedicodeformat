unit TestAsmOptionsCaps;

interface

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform;

type
  TTestAsmOptionsCaps = class(TBaseTestProcess)
  private
    fbBreaksAfterLabelEnabled: boolean;
    fbIndentsEnabled: boolean;
    feCapitalisation: TCapitalisationType;
    fiBreaksAfterLabel: integer;
  public
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

  ASM_STATEMENTS_UPPER =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    MOV   ECX, [EDX]' + AnsiLineBreak +
    '    XCHG  ECX, [EAX]' + AnsiLineBreak +
    '    CALL    PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_LOWER =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    mov   ecx, [edx]' + AnsiLineBreak +
    '    xchg  ecx, [eax]' + AnsiLineBreak +
    '    call    procasm2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;


    ASM_STATEMENTS_MIXED =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    MOV   ecx, [EDX]' + AnsiLineBreak +
    '    xchg  ECX, [eax]' + AnsiLineBreak +
    '    CALL    Procasm2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_INITALCAPS =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '    Mov   Ecx, [Edx]' + AnsiLineBreak +
    '    Xchg  Ecx, [Eax]' + AnsiLineBreak +
    '    Call    Procasm2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;


procedure TTestAsmOptionsCaps.SetUp;
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

procedure TTestAsmOptionsCaps.TearDown;
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


procedure TTestAsmOptionsCaps.TestCaps_UpperLeaveAlone;
begin
  FormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToLower;
begin
  FormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToMixed;
begin
  FormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_UpperToUpper;
begin
  FormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_UPPER, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedLeaveAlone;
begin
  FormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_MIXED);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToLower;
begin
  FormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToMixed;
begin
  FormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_MixedToUpper;
begin
  FormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_MIXED, ASM_STATEMENTS_UPPER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerLeaveAlone;
begin
  FormatSettings.SetAsm.Capitalisation := ctLeaveAlone;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToLower;
begin
  FormatSettings.SetAsm.Capitalisation := ctLower;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_LOWER);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToMixed;
begin
  FormatSettings.SetAsm.Capitalisation := ctMixed;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_INITALCAPS);
end;

procedure TTestAsmOptionsCaps.TestCaps_LowerToUpper;
begin
  FormatSettings.SetAsm.Capitalisation := ctUpper;
  TestFormatResult(ASM_STATEMENTS_LOWER, ASM_STATEMENTS_UPPER);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsCaps.Suite);
end.
