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
------------------------------------------------------------------------------*)
{*)}

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
    fbSaveBreakUsesSortOnReturn: Boolean;
    fbSaveBreakUsesSortOnComment: Boolean;
    feSaveUsesSortOrder: TUsesSortOrder;
    fbSaveSortUsesNoComments: boolean;

  protected
    procedure Setup; override;
    procedure Teardown; override;

  published

    procedure TestEmpty;
    procedure Test1;
    procedure Test2;
    procedure Test2_outofOrder;

    procedure Test3;
    procedure Test3_outofOrder;
    procedure Test10_outofOrder;

    procedure TestSectionsReturn1;
    procedure TestSectionsReturn2;
    procedure TestSectionsReturn3;

    procedure TestComment1;
    procedure TestComment2;
    procedure TestComment3;

    procedure TestSortByCommentSection1;
    procedure TestSortByCommentSection2;

    procedure TestIfDef1;
    procedure TestIfDef2;
  end;

implementation

uses
  JclStrings,
  SortUses,
  JcfSettings;

const
  TEST_UNIT_START =
    'unit TestUnit;' + AnsiLineBreak + AnsiLineBreak +
    'interface' + AnsiLineBreak;

TEST_UNIT_END =
    AnsiLineBreak +
    'implementation' + AnsiLineBreak + AnsiLineBreak +
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
    ' uses aUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSortUses.Test2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;


procedure TTestSortUses.Test2_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, aUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, IN_UNIT_TEXT);
end;

procedure TTestSortUses.Test3_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.Test10_outofOrder;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses dUnit, cUnit, wUnit, zUnit, fUnit, aUnit, bUnit, jUnit, nUnit, pUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, cUnit, dUnit, fUnit, jUnit, nUnit, pUnit, wUnit, zUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSectionsReturn1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit,' + AnsiLineBreak +
    ' zUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, cUnit, ' + AnsiLineBreak +
    ' bUnit, zUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSectionsReturn2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit,' + AnsiLineBreak +
    ' zUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit,' + AnsiLineBreak +
    ' bUnit, zUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);end;

procedure TTestSortUses.TestSectionsReturn3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses cUnit, aUnit,' + AnsiLineBreak +
    ' fUnit,' + AnsiLineBreak +
    ' zUnit, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, cUnit, ' + AnsiLineBreak +
    ' fUnit,' + AnsiLineBreak +
    ' bUnit, zUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnReturn := True;
  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit {a comment}, aUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit {a comment};' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, aUnit {a comment};' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit {a comment}, bUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestComment3;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bUnit, // a comment' + AnsiLineBreak +
    ' aUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses aUnit, bUnit, // a comment' + AnsiLineBreak +
    ';' + AnsiLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSortByCommentSection1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + AnsiLineBreak +
    ' zDUnit,' + AnsiLineBreak +
    ' fDUnit,' + AnsiLineBreak +
    ' { new section }' + AnsiLineBreak +
    ' gUnit,' + AnsiLineBreak +
    ' aUnit,' + AnsiLineBreak +
    ' cUnit; ' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + AnsiLineBreak +
    ' fDUnit,' + AnsiLineBreak +
    ' zDUnit,' + AnsiLineBreak +
    ' { new section }' + AnsiLineBreak +
    ' aUnit,' + AnsiLineBreak +
    ' cUnit, gUnit; ' + AnsiLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnComment := True;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestSortByCommentSection2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + AnsiLineBreak +
    ' zDUnit,' + AnsiLineBreak +
    ' fDUnit,' + AnsiLineBreak +
    ' // new section' + AnsiLineBreak +
    ' gUnit,' + AnsiLineBreak +
    ' aUnit,' + AnsiLineBreak +
    ' cUnit; ' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    ' uses bDUnit,' + AnsiLineBreak +
    ' fDUnit,' + AnsiLineBreak +
    ' zDUnit,' + AnsiLineBreak +
    ' // new section' + AnsiLineBreak +
    ' aUnit,' + AnsiLineBreak +
    ' cUnit, gUnit; ' + AnsiLineBreak + TEST_UNIT_END;
begin
  SetTestSortState;
  FormatSettings.Transform.BreakUsesSortOnComment := True;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestIfDef1;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    'uses' + AnsiLineBreak +
    '  aZUnit, qZUnit, bZUnit, fZUNit,' + AnsiLineBreak +
    '  {$IFDEF foo}' +  AnsiLineBreak +
    '  aUnit, gUnit, bUnit;' + AnsiLineBreak +
    '  {$ELSE}' +  AnsiLineBreak +
    '  aQUnit, gQUnit, bQUnit;' + AnsiLineBreak +
    '  {$ENDIF}' + AnsiLineBreak +
    TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    'uses' + AnsiLineBreak +
    '  aZUnit, bZUnit, fZUNit,' + AnsiLineBreak +
    '  qZUnit, {$IFDEF foo}' + AnsiLineBreak +
    '  aUnit, gUnit, bUnit;' + AnsiLineBreak +
    '  {$ELSE}' + AnsiLineBreak +
    '  aQUnit, bQUnit, gQUnit;' + AnsiLineBreak +
    '  {$ENDIF}' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSortUses.TestIfDef2;
const
  IN_UNIT_TEXT = TEST_UNIT_START +
    'uses' + AnsiLineBreak +
    'aZUnit, qZUnit, bZUnit, fZUNit,' + AnsiLineBreak +
    '{$IFDEF foo}' + AnsiLineBreak +
    'aUnit, gUnit, bUnit,' + AnsiLineBreak +
    '{$ELSE}' + AnsiLineBreak +
    'aQUnit, gQUnit, bQUnit,' + AnsiLineBreak +
    '{$ENDIF}' + AnsiLineBreak +
    'xMUnit, gMUnit, rMUnit;' + AnsiLineBreak +
  TEST_UNIT_END;
  OUT_UNIT_TEXT = TEST_UNIT_START +
    'uses' + AnsiLineBreak +
    'aZUnit, bZUnit, fZUNit,' + AnsiLineBreak +
    'qZUnit, {$IFDEF foo}' + AnsiLineBreak +
    'aUnit, gUnit, bUnit,' + AnsiLineBreak +
    '{$ELSE}' + AnsiLineBreak +
    'aQUnit, bQUnit,' + AnsiLineBreak +
    'gQUnit, {$ENDIF}' + AnsiLineBreak +
    'gMUnit, rMUnit, xMUnit;' + AnsiLineBreak +
    TEST_UNIT_END;
begin
  SetTestSortState;

  TestProcessResult(TSortUses, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestSortUses.Suite);
end.
