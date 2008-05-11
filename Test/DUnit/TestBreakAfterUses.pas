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
    fbBreakAfterUses: boolean;
    fbUsesClauseOnePerLine: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestNoChange;
    procedure TestBreakAfterUses;
    procedure TestUsesClauseBoth;
    procedure TestUsesClauseOnePerLine;

  end;

implementation

uses
  JclStrings,
  JcfSettings,
  ReturnAfter;

const
  INTERFACE_HEADER = 'unit Test;' + AnsiLineBreak + AnsiLineBreak +
    'interface' + AnsiLineBreak + AnsiLineBreak;

  IMPLEMENTATION_SECTION =
    'implementation' + AnsiLineBreak + AnsiLineBreak +
    'end.';

  USES_ONE_LINE = 'uses foo, bar;' + AnsiLineBreak + AnsiLineBreak;
  USES_BREAK_AFTER_USES = 'uses' + AnsiLineBreak + 'foo, bar;' + AnsiLineBreak + AnsiLineBreak;
  USES_ONE_PER_LINE = 'uses' + AnsiLineBreak + 'foo,' + AnsiLineBreak + 'bar;' + AnsiLineBreak + AnsiLineBreak;

  UNIT_ALL_ONE_LINE =
    INTERFACE_HEADER + USES_ONE_LINE + IMPLEMENTATION_SECTION;

  UNIT_BREAK_AFTER_USES =
    INTERFACE_HEADER + USES_BREAK_AFTER_USES + IMPLEMENTATION_SECTION;

  UNIT_ONE_PER_LINE =
    INTERFACE_HEADER + USES_ONE_PER_LINE + IMPLEMENTATION_SECTION;

procedure TTestBreakAfterUses.SetUp;
begin
  inherited;

  fbBreakAfterUses := FormatSettings.Returns.BreakAfterUses;
  fbUsesClauseOnePerLine := FormatSettings.Returns.UsesClauseOnePerLine;

end;

procedure TTestBreakAfterUses.TearDown;
begin
  inherited;

  FormatSettings.Returns.BreakAfterUses := fbBreakAfterUses;
  FormatSettings.Returns.UsesClauseOnePerLine := fbUsesClauseOnePerLine;

end;

procedure TTestBreakAfterUses.TestNoChange;
begin
  FormatSettings.Returns.BreakAfterUses := False;
  FormatSettings.Returns.UsesClauseOnePerLine := False;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ALL_ONE_LINE);

end;

procedure TTestBreakAfterUses.TestBreakAfterUses;
begin
  FormatSettings.Returns.BreakAfterUses := True;
  FormatSettings.Returns.UsesClauseOnePerLine := False;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_BREAK_AFTER_USES);
end;

procedure TTestBreakAfterUses.TestUsesClauseOnePerLine;
begin
  FormatSettings.Returns.BreakAfterUses := False;
  FormatSettings.Returns.UsesClauseOnePerLine := True;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;

procedure TTestBreakAfterUses.TestUsesClauseBoth;
begin
  FormatSettings.Returns.BreakAfterUses := True;
  FormatSettings.Returns.UsesClauseOnePerLine := True;

  // processes are turned off, no change
  TestProcessResult(TReturnAfter, UNIT_ALL_ONE_LINE, UNIT_ONE_PER_LINE);
end;


initialization
  TestFramework.RegisterTest('Processes', TTestBreakAfterUses.Suite);
end.
