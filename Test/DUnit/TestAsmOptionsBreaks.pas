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

    procedure TestLabelBreaksNone_None;
    procedure TestLabelBreaksNone_One;
    procedure TestLabelBreaksNone_Three;
    procedure TestLabelBreaksNone_Two;

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
    '    @@testLabel: MOV   ECX, [EDX]' + AnsiLineBreak +
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


initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsBreaks.Suite);
end.
