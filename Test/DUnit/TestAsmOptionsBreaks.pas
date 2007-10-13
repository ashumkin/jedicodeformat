unit TestAsmOptionsBreaks;

interface

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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestLabelBreaksNone;

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


procedure TTestAsmOptionsBreaks.TestLabelBreaksNone;
begin
  FormatSettings.SetAsm.BreaksAfterLabelEnabled := True;
  FormatSettings.SetAsm.BreaksAfterLabel := 0;
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsBreaks.Suite);
end.
