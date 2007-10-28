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
------------------------------------------------------------------------------*)
{*)}

interface

{ test asm options for indents }

uses
  TestFrameWork,
  BaseTestProcess, SettingsTypes, SetTransform, IntList;

type

  TTestAsmOptionsIndents = class(TBaseTestProcess)
  private
    fbBreaksAfterLabelEnabled: boolean;
    fbIndentsEnabled: boolean;
    feCapitalisation: TCapitalisationType;
    fiBreaksAfterLabel: integer;
    fcStoreIndents: TIntList;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    destructor Destroy; override;

  published
    procedure TestIndent;
  end;

implementation

uses
  SysUtils,
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

  ASM_STATEMENTS_NOT_INDENTED =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    'MOV ECX, [EDX]' + AnsiLineBreak +
    'XCHG ECX, [EAX]' + AnsiLineBreak +
    'CALL PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;

    ASM_STATEMENTS_INDENTED =
    UNIT_HEADER +
    '  asm' + AnsiLineBreak +
    '  MOV   ECX, [EDX]' + AnsiLineBreak +
    '  XCHG  ECX, [EAX]' + AnsiLineBreak +
    '  CALL  PROCASM2' + AnsiLineBreak +
    '  end;' + AnsiLineBreak +
    UNIT_FOOTER;


destructor TTestAsmOptionsIndents.Destroy;
begin
  FreeAndNil(fcStoreIndents);
  inherited;
end;

procedure TTestAsmOptionsIndents.SetUp;
var
  liIndentLoop: integer;
begin
  inherited;


  // store old settings
  with FormatSettings do
  begin
    fbBreaksAfterLabelEnabled := SetAsm.BreaksAfterLabelEnabled;
    fbIndentsEnabled := SetAsm.IndentsEnabled;
    feCapitalisation := SetAsm.Capitalisation;
    fiBreaksAfterLabel := SetAsm.BreaksAfterLabel;

    if fcStoreIndents = nil then
      fcStoreIndents := TIntList.Create;

    fcStoreIndents.Clear;
    for liIndentLoop := 0 to 5 do
    begin
      fcStoreIndents.Add(SetAsm.Indents[liIndentLoop]);
    end;
  end;
end;

procedure TTestAsmOptionsIndents.TearDown;
var
  liIndentLoop: integer;
begin
  inherited;

  with FormatSettings do
  begin
    SetAsm.BreaksAfterLabelEnabled := fbBreaksAfterLabelEnabled;
    SetAsm.IndentsEnabled := fbIndentsEnabled;
    SetAsm.Capitalisation := feCapitalisation;
    SetAsm.BreaksAfterLabel := fiBreaksAfterLabel;

    for liIndentLoop := 0 to 5 do
    begin
      SetAsm.Indents[liIndentLoop] := fcStoreIndents.Items[liIndentLoop];
    end;
  end;

end;

procedure TTestAsmOptionsIndents.TestIndent;
begin
 // code not working yet  TestFormatResult(ASM_STATEMENTS_NOT_INDENTED, ASM_STATEMENTS_INDENTED);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestAsmOptionsIndents.Suite);

end.
