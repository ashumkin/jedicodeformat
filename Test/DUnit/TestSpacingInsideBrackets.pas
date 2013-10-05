unit TestSpacingInsideBrackets;

{ AFS Jan 2003
  Test the spacing processes }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestSpacing, released May 2003.
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

uses
  TestFrameWork,
  BaseTestProcess,
  SettingsTypes;

type
  TTestSpacingInsideBrackets = class(TBaseTestProcess)
  private
    fbSpaceAfterOpen: boolean;
    fbSpaceBeforeClose: boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestNoSpaceAfterBegin;
    procedure TestSpaceAfterBegin;

    procedure TestAddSpaces;
    procedure TestSpacesPresent;

  end;

implementation

uses
  { local }
  JcfStringUtils,
  JcfSettings,
  NoReturnAfter, NoReturnBefore, NoSpaceAfter, NoSpaceBefore,
  SpaceBeforeColon,
  SingleSpaceBefore, SingleSpaceAfter,
  ReturnBefore, ReturnAfter, RemoveBlankLinesAfterProcHeader,
  RemoveReturnsAfterBegin, RemoveReturnsBeforeEnd, UnitNameCaps,
  TabToSpace, SpaceToTab, MaxSpaces, SetSpaces;

procedure TTestSpacingInsideBrackets.Setup;
begin
  inherited;

  fbSpaceAfterOpen := JcfFormatSettings.Spaces.SpaceAfterOpenBrackets;
  fbSpaceBeforeClose := JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets;
end;

procedure TTestSpacingInsideBrackets.Teardown;
begin
  inherited;

 JcfFormatSettings.Spaces.SpaceAfterOpenBrackets := fbSpaceAfterOpen;
 JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets := fbSpaceBeforeClose;
end;

const
  NO_SPACES_UNIT_TEXT =
    UNIT_HEADER +
    ' procedure foo; begin if (foo) then ; end; ' +
    UNIT_FOOTER;
  SPACE_AFTER_BEGIN_UNIT_TEXT =
    UNIT_HEADER +
    ' procedure foo; begin if ( foo) then ; end; ' +
    UNIT_FOOTER;
  SPACE_BEFORE_END_UNIT_TEXT =
    UNIT_HEADER +
    ' procedure foo; begin if (foo ) then ; end; ' +
    UNIT_FOOTER;
  BOTH_SPACE_UNIT_TEXT =
    UNIT_HEADER +
    ' procedure foo; begin if ( foo ) then ; end; ' +
    UNIT_FOOTER;


procedure TTestSpacingInsideBrackets.TestNoSpaceAfterBegin;
begin
 JcfFormatSettings.Spaces.SpaceAfterOpenBrackets := False;
 JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets := False;

  // processes are turned off, brackets stay put
  TestProcessResult(TSingleSpaceAfter, NO_SPACES_UNIT_TEXT, NO_SPACES_UNIT_TEXT);

  TestProcessResult(TSingleSpaceBefore, NO_SPACES_UNIT_TEXT, NO_SPACES_UNIT_TEXT);
end;

procedure TTestSpacingInsideBrackets.TestSpaceAfterBegin;
begin
 JcfFormatSettings.Spaces.SpaceAfterOpenBrackets := False;
 JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets := False;

  // processes  are turned off, brackets stay put
  TestProcessResult(TSingleSpaceAfter, SPACE_AFTER_BEGIN_UNIT_TEXT, SPACE_AFTER_BEGIN_UNIT_TEXT);
  TestProcessResult(TSingleSpaceBefore, SPACE_AFTER_BEGIN_UNIT_TEXT, SPACE_AFTER_BEGIN_UNIT_TEXT);
end;

procedure TTestSpacingInsideBrackets.TestAddSpaces;
begin
 JcfFormatSettings.Spaces.SpaceAfterOpenBrackets := True;
 JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets := True;

  // space-after process is turned on, space inserted
  TestProcessResult(TSingleSpaceAfter, NO_SPACES_UNIT_TEXT, SPACE_AFTER_BEGIN_UNIT_TEXT);

  // space-after process is turned on, space inserted
  TestProcessResult(TSingleSpaceBefore, NO_SPACES_UNIT_TEXT, SPACE_BEFORE_END_UNIT_TEXT);
end;

procedure TTestSpacingInsideBrackets.TestSpacesPresent;
begin
 JcfFormatSettings.Spaces.SpaceAfterOpenBrackets := True;
 JcfFormatSettings.Spaces.SpaceBeforeCloseBrackets := True;

  // no change here
  TestProcessResult(TSingleSpaceBefore, BOTH_SPACE_UNIT_TEXT, BOTH_SPACE_UNIT_TEXT);

  // space-before process is turned on, space inserted
  TestProcessResult(TSingleSpaceAfter, BOTH_SPACE_UNIT_TEXT, BOTH_SPACE_UNIT_TEXT);

end;

initialization
  TestFramework.RegisterTest('Processes', TTestSpacingInsideBrackets.Suite);
end.
