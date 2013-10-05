unit TestBreakAfterUses;

{ AFS Jan April 2008
  Test breakingafter uses }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestBreakAfterUses, released April 2008
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2008 Anthony Steele.
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
uses
  TestFrameWork,
  BaseTestProcess,
  SettingsTypes;

type
  TTestBreakAfterUses = class(TBaseTestProcess)
  private
    fbAddGoodReturns: boolean;
    fbBreakAfterUses: boolean;
    fbUsesClauseOnePerLine: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    { there are 3 boolean vars -
      these cover all 8 possible combinations of options }
    procedure TestAllOff;
    procedure TestNoChange;
    procedure TestBreakAfterUses;
    procedure TestBreakAfterUsesAndGoodReturns;
    procedure TestUsesClauseAll;
    procedure TestUsesClauseOnePerLine;
    procedure TestUsesSpacing;
    procedure TestUsesClauseOnePerLineNoOtherReturns;

  end;

implementation

uses
  JcfStringUtils,
  JcfSettings,
  ReturnAfter;

const
  INTERFACE_HEADER = 'unit Test;' + NativeLineBreak + NativeLineBreak +
    'interface' + NativeLineBreak + NativeLineBreak;

  IMPLEMENTATION_SECTION =
    'implementation' + NativeLineBreak + NativeLineBreak +
    'end.';

  USES_ONE_LINE = 'uses foo, bar;' + NativeLineBreak + NativeLineBreak;
  USES_BREAK_AFTER_USES = 'uses' + NativeLineBreak + 'foo, bar;' + NativeLineBreak + NativeLineBreak;
  USES_ONE_PER_LINE = 'uses' + NativeLineBreak + 'foo,' + NativeLineBreak + 'bar;' + NativeLineBreak + NativeLineBreak;

  UNIT_ALL_ONE_LINE =
    INTERFACE_HEADER + USES_ONE_LINE + IMPLEMENTATION_SECTION;

  UNIT_BREAK_AFTER_USES =
    INTERFACE_HEADER + USES_BREAK_AFTER_USES + IMPLEMENTATION_SECTION;

  UNIT_ONE_PER_LINE =
    INTERFACE_HEADER + USES_ONE_PER_LINE + IMPLEMENTATION_SECTION;

procedure TTestBreakAfterUses.SetUp;
begin
  inherited;

  fbAddGoodReturns := JcfFormatSettings.Returns.AddGoodReturns;
  fbBreakAfterUses := JcfFormatSettings.Returns.BreakAfterUses;
  fbUsesClauseOnePerLine := JcfFormatSettings.Returns.UsesClauseOnePerLine;

end;

procedure TTestBreakAfterUses.TearDown;
begin
  inherited;

 JcfFormatSettings.Returns.AddGoodReturns := fbAddGoodReturns;
 JcfFormatSettings.Returns.BreakAfterUses := fbBreakAfterUses;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := fbUsesClauseOnePerLine;

end;

procedure TTestBreakAfterUses.TestAllOff;
begin
 JcfFormatSettings.Returns.AddGoodReturns := False;
 JcfFormatSettings.Returns.BreakAfterUses := False;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := False;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ALL_ONE_LINE);

end;


procedure TTestBreakAfterUses.TestNoChange;
begin
 JcfFormatSettings.Returns.AddGoodReturns := True;
 JcfFormatSettings.Returns.BreakAfterUses := False;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := False;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ALL_ONE_LINE);

end;

procedure TTestBreakAfterUses.TestBreakAfterUses;
begin
 JcfFormatSettings.Returns.AddGoodReturns := False;
 JcfFormatSettings.Returns.BreakAfterUses := True;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := False;

  // break after uses is applied
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_BREAK_AFTER_USES);
end;

procedure TTestBreakAfterUses.TestUsesSpacing;
begin
 JcfFormatSettings.Returns.AddGoodReturns := False;
 JcfFormatSettings.Returns.BreakAfterUses := True;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := True;

  // break after uses is applied
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;


procedure TTestBreakAfterUses.TestBreakAfterUsesAndGoodReturns;
begin
 JcfFormatSettings.Returns.AddGoodReturns := True;
 JcfFormatSettings.Returns.BreakAfterUses := True;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := False;

  // break after uses is applied
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_BREAK_AFTER_USES);
end;

procedure TTestBreakAfterUses.TestUsesClauseOnePerLine;
begin
 JcfFormatSettings.Returns.AddGoodReturns := True;
 JcfFormatSettings.Returns.BreakAfterUses := False;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := True;

  // uses clause on one line
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;

procedure TTestBreakAfterUses.TestUsesClauseOnePerLineNoOtherReturns;
begin
 JcfFormatSettings.Returns.AddGoodReturns := False;
 JcfFormatSettings.Returns.BreakAfterUses := False;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := True;

  // uses clause on one line
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;


procedure TTestBreakAfterUses.TestUsesClauseAll;
begin
 JcfFormatSettings.Returns.AddGoodReturns := True;
 JcfFormatSettings.Returns.BreakAfterUses := True;
 JcfFormatSettings.Returns.UsesClauseOnePerLine := True;

  // all options are on
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;


initialization
  TestFramework.RegisterTest('Processes', TTestBreakAfterUses.Suite);
end.
