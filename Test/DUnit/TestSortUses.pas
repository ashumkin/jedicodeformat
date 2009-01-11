unit TestSortUses;

(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestSortUses, released Sept 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
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

{ AFS 12 Sept 04
 Test the uses sort }

uses
  TestFrameWork,
  BaseTestProcess,
  SetTransform;

type
  TTestSortUses = class(TBaseTestProcess)
  private
    fbSaveSortInterfaceUses: Boolean;
    fbSaveSortImplementationUses: Boolean;
    fbSaveSortProgramUses: Boolean;
    fbSaveBreakUsesSortOnReturn: Boolean;
    fbSaveBreakUsesSortOnComment: Boolean;
    feSaveUsesSortOrder: TUsesSortOrder;
    fbSaveSortUsesNoComments: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestEmpty;
    procedure Test1;
    procedure Test2;
    procedure Test2_outofOrder;

    procedure Test2_outofOrder_DotNet;
    procedure Test2_outofOrder_DotNet2;
    procedure Test4_outofOrder_DotNet;
    procedure Test6_outofOrder_DotNet;

    procedure Test3;
    procedure Test3_outofOrder;
    procedure Test10_outofOrder;

    procedure TestSectionsReturn1;
    procedure TestSectionsReturn2;
    procedure TestSectionsReturn3;

    procedure TestSectionsReturnComma;

    procedure TestComment1;
    procedure TestComment2;
    procedure TestComment3;

    procedure TestSortByCommentSection1;
    procedure TestSortByCommentSection2;

    procedure TestProgram;

    procedure TestIfDef1;
    procedure TestIfDef2;
  end;

implementation

uses
  JcfStringUtils,
  SortUses,
  JcfSettings;

const
  TEST_UNIT_START =
    'unit TestUnit;' + NativeLineBreak + NativeLineBreak +
    'interface' + NativeLineBreak;

TEST_UNIT_END =
    NativeLineBreak +
    'implementation' + NativeLineBreak + NativeLineBreak +
    'end.';

procedure SetTestSortState;
begin
  FormatSettings.Transform.SortInterfaceUses := True;
  FormatSettings.Transform.UsesSortOrder := eAlpha;
  FormatSettings.Transform.BreakUsesSortOnReturn := False;
  FormatSettings.Transform.BreakUsesSortOnComment := False;
end;

procedure TTestSortUses.Setup;
begin
  inherited;

  { save the sort uses state before we monkey with it }
  fbSaveSortInterfaceUses := FormatSettings.Transform.SortInterfaceUses;
  fbSaveSortImplementationUses := FormatSettings.Transform.SortImplementationUses;
  fbSaveSortProgramUses := FormatSettings.Transform.SortProgramUses;

  fbSaveBreakUsesSortOnReturn := FormatSettings.Transform.BreakUsesSortOnReturn;
  fbSaveBreakUsesSortOnComment := FormatSettings.Transform.BreakUsesSortOnComment;
  feSaveUsesSortOrder := FormatSettings.Transform.UsesSortOrder;
  fbSaveSortUsesNoComments := FormatSettings.Transform.SortUsesNoComments;
end;

procedure TTestSortUses.Teardown;
begin
  { restore sort uses state }
  FormatSettings.Transform.SortInterfaceUses := fbSaveSortInterfaceUses;
  FormatSettings.Transform.SortImplementationUses := fbSaveSortImplementationUses;
  FormatSettings.Transform.SortProgramUses := fbSaveSortProgramUses;

  FormatSettings.Transform.BreakUsesSortOnReturn := fbSaveBreakUsesSortOnReturn;
  FormatSettings.Transform.BreakUsesSortOnComment := fbSaveBreakUsesSortOnComment;
  FormatSettings.Transform.UsesSortOrder := feSaveUsesSortOrder;
  fbSaveSortUsesNoComments := FormatSettings.Transform.SortUsesNoComments;

  inherited;
end;

procedure TTestSortUses.TestEmpty;
const
  IN_UNIT_TEXT = TEST_UNIT_START + TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSortUses.Test1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSortUses.Test2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;


procedure TTestSortUses.Test2_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test2_outofOrder_DotNet;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses Foo.bUnit, Foo.aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses Foo.aUnit, Foo.bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test2_outofOrder_DotNet2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses Foo.bUnit, Bar.bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses Bar.bUnit, Foo.bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test4_outofOrder_DotNet;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses Foo.bUnit, Bar.aUnit, Bar.bUnit, Foo.aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses Bar.aUnit, Bar.bUnit, Foo.aUnit, Foo.bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestSortUses.Test6_outofOrder_DotNet;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses Zed.Bee, Foo.bUnit, Bar.aUnit, Bar.bUnit, Foo.aUnit, System.Type;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses Bar.aUnit, Bar.bUnit, Foo.aUnit, Foo.bUnit, System.Type, Zed.Bee;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSortUses.Test3_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test10_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses dUnit, cUnit, wUnit, zUnit, fUnit, aUnit, bUnit, jUnit, nUnit, pUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit, dUnit, fUnit, jUnit, nUnit, pUnit, wUnit, zUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSectionsReturn1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit,' + NativeLineBreak +
    ' zUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, cUnit, ' + NativeLineBreak +
    ' bUnit, zUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSectionsReturn2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit,' + NativeLineBreak +
    ' zUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit,' + NativeLineBreak +
    ' bUnit, zUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);end;

procedure TTestSortUses.TestSectionsReturn3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit,' + NativeLineBreak +
    ' fUnit,' + NativeLineBreak +
    ' zUnit, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, cUnit, ' + NativeLineBreak +
    ' fUnit,' + NativeLineBreak +
    ' bUnit, zUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

{ Test for bug 1423817 - exta comma added at end of line
  before comma at the start of the next line }
procedure TTestSortUses.TestSectionsReturnComma;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' Uses SysUtils {Exception}' + NativeLineBreak +
    ', WinTypes' +
    ';' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' Uses SysUtils {Exception}, ' +  NativeLineBreak +
    ' WinTypes' +
    ';' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit {a comment}, aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit {a comment};' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, aUnit {a comment};' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit {a comment}, bUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, // a comment' + NativeLineBreak +
    ' aUnit;' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, // a comment' + NativeLineBreak +
    ';' + NativeLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSortByCommentSection1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + NativeLineBreak +
    ' zDUnit,' + NativeLineBreak +
    ' fDUnit,' + NativeLineBreak +
    ' { new section }' + NativeLineBreak +
    ' gUnit,' + NativeLineBreak +
    ' aUnit,' + NativeLineBreak +
    ' cUnit; ' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + NativeLineBreak +
    ' fDUnit,' + NativeLineBreak +
    ' zDUnit,' + NativeLineBreak +
    ' { new section }' + NativeLineBreak +
    ' aUnit,' + NativeLineBreak +
    ' cUnit, gUnit; ' + NativeLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnComment := True;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSortByCommentSection2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + NativeLineBreak +
    ' zDUnit,' + NativeLineBreak +
    ' fDUnit,' + NativeLineBreak +
    ' // new section' + NativeLineBreak +
    ' gUnit,' + NativeLineBreak +
    ' aUnit,' + NativeLineBreak +
    ' cUnit; ' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + NativeLineBreak +
    ' fDUnit,' + NativeLineBreak +
    ' zDUnit,' + NativeLineBreak +
    ' // new section' + NativeLineBreak +
    ' aUnit,' + NativeLineBreak +
    ' cUnit, gUnit; ' + NativeLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnComment := True;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestIfDef1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    'uses' + NativeLineBreak +
    '  aZUnit, qZUnit, bZUnit, fZUNit,' + NativeLineBreak +
    '  {$IFDEF foo}' +  NativeLineBreak +
    '  aUnit, gUnit, bUnit;' + NativeLineBreak +
    '  {$ELSE}' +  NativeLineBreak +
    '  aQUnit, gQUnit, bQUnit;' + NativeLineBreak +
    '  {$ENDIF}' + NativeLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    'uses' + NativeLineBreak +
    '  aZUnit, bZUnit, fZUNit,' + NativeLineBreak +
    '  qZUnit, {$IFDEF foo}' + NativeLineBreak +
    '  aUnit, gUnit, bUnit;' + NativeLineBreak +
    '  {$ELSE}' + NativeLineBreak +
    '  aQUnit, bQUnit, gQUnit;' + NativeLineBreak +
    '  {$ENDIF}' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestIfDef2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    'uses' + NativeLineBreak +
    'aZUnit, qZUnit, bZUnit, fZUNit,' + NativeLineBreak +
    '{$IFDEF foo}' + NativeLineBreak +
    'aUnit, gUnit, bUnit,' + NativeLineBreak +
    '{$ELSE}' + NativeLineBreak +
    'aQUnit, gQUnit, bQUnit,' + NativeLineBreak +
    '{$ENDIF}' + NativeLineBreak +
    'xMUnit, gMUnit, rMUnit;' + NativeLineBreak +
  TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    'uses' + NativeLineBreak +
    'aZUnit, bZUnit, fZUNit,' + NativeLineBreak +
    'qZUnit, {$IFDEF foo}' + NativeLineBreak +
    'aUnit, gUnit, bUnit,' + NativeLineBreak +
    '{$ELSE}' + NativeLineBreak +
    'aQUnit, bQUnit,' + NativeLineBreak +
    'gQUnit, {$ENDIF}' + NativeLineBreak +
    'gMUnit, rMUnit, xMUnit;' + NativeLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestProgram;
const
  IN_UNIT_TEXT =
  ' program Project1;' + NativeLineBreak + NativeLineBreak +
  '{$APPTYPE CONSOLE}' + NativeLineBreak + NativeLineBreak +
  'uses' + NativeLineBreak +
  '  SysUtils, Windows, Controls, Messages; ' +  NativeLineBreak + NativeLineBreak +
  'begin' + NativeLineBreak +
  '  Writeln(''Hello World !'');' + NativeLineBreak +
  'end.';

  OUT_UNIT_TEXT =
  ' program Project1;' + NativeLineBreak + NativeLineBreak +
  '{$APPTYPE CONSOLE}' + NativeLineBreak + NativeLineBreak +
  'uses' + NativeLineBreak +
  '  Controls, Messages, SysUtils, Windows; ' +  NativeLineBreak + NativeLineBreak +
  'begin' + NativeLineBreak +
  '  Writeln(''Hello World !'');' + NativeLineBreak +
  'end.';

begin
  SetTestSortState;
  FormatSettings.Transform.SortProgramUses := False;

  { no sorting for a program uses clause }
  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);

  { now it will be sorted }
  FormatSettings.Transform.SortProgramUses := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestSortUses.Suite);
end.
