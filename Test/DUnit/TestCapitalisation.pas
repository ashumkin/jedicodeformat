unit TestCapitalisation;


{ AFS June 2005
  Test the caps processe }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestCapitalisation, released June 2005.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2005 Anthony Steele.
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

uses
  TestFrameWork,
  BaseTestProcess,
  SettingsTypes;

type
  TTestCapitalisation = class(TBaseTestProcess)
  private
    fbSaveEnabled: boolean;
    feSaveReservedWords: TCapitalisationType;
    feSaveOperators: TCapitalisationType;
    feSaveDirectives: TCapitalisationType;
    feSaveConstants: TCapitalisationType;
    feSaveTypes: TCapitalisationType;

    fbSaveAnyWordCapsEnabled: boolean;

  protected
    procedure Setup; override;
    procedure Teardown; override;

  published

    procedure TestPropertyCapsNull;
    procedure TestPropertyCapsUpper;
    procedure TestPropertyCapsMixed;
    procedure TestPropertyCapsLower;

  end;

implementation

uses
  { Jcl }
  JclStrings,
  { local }
  Capitalisation, JcfSettings, SetCaps;


procedure TTestCapitalisation.Setup;
var
  lcSetCaps: TSetCaps;
begin
  inherited;

  lcSetCaps := FormatSettings.Caps;

  fbSaveEnabled := lcSetCaps.Enabled;
  feSaveReservedWords := lcSetCaps.ReservedWords;
  feSaveOperators := lcSetCaps.Operators;
  feSaveDirectives := lcSetCaps.Directives;
  feSaveConstants := lcSetCaps.Constants;
  feSaveTypes := lcSetCaps.Types;

  fbSaveAnyWordCapsEnabled := FormatSettings.SpecificWordCaps.Enabled;

  // defualt setup
  FormatSettings.Caps.Enabled := True;
  FormatSettings.SpecificWordCaps.Enabled := False;
end;

procedure TTestCapitalisation.Teardown;
var
  lcSetCaps: TSetCaps;
begin
  inherited;

  lcSetCaps := FormatSettings.Caps;

  lcSetCaps.Enabled := fbSaveEnabled;
  lcSetCaps.ReservedWords := feSaveReservedWords;
  lcSetCaps.Operators := feSaveOperators;
  lcSetCaps.Directives := feSaveDirectives;
  lcSetCaps.Constants := feSaveConstants;
  lcSetCaps.Types := feSaveTypes;

  FormatSettings.SpecificWordCaps.Enabled := fbSaveAnyWordCapsEnabled;
end;


const
  LOWER_PROPERTY =
    'private' + AnsiLineBreak +
    'fiFoo: integer;' + AnsiLineBreak +
    'public' + AnsiLineBreak +
    ' property foo read fiFoo write fiFoo;' + AnsiLineBreak;

  UPPER_PROPERTY =
    'PRIVATE' + AnsiLineBreak +
    'fiFoo: integer;' + AnsiLineBreak +
    'PUBLIC' + AnsiLineBreak +
      ' property foo READ fiFoo WRITE fiFoo;' + AnsiLineBreak;

  MIXED_PROPERTY =
    'Private' + AnsiLineBreak +
    'fiFoo: integer;' + AnsiLineBreak +
    'Public' + AnsiLineBreak +
      ' property foo Read fiFoo Write fiFoo;' + AnsiLineBreak;

  UNIT_TEXT_PREFIX  = UNIT_HEADER +
    'type TFoo = class' + AnsiLineBreak;

  UNIT_TEXT_SUFFIX = 'end;' + AnsiLineBreak +
    UNIT_FOOTER;

  LOWER_UNIT = UNIT_TEXT_PREFIX + LOWER_PROPERTY + UNIT_TEXT_SUFFIX;
  UPPER_UNIT = UNIT_TEXT_PREFIX + UPPER_PROPERTY + UNIT_TEXT_SUFFIX;
  MIXED_UNIT = UNIT_TEXT_PREFIX + MIXED_PROPERTY + UNIT_TEXT_SUFFIX;



procedure TTestCapitalisation.TestPropertyCapsNull;
begin
  // stay the same
  FormatSettings.Caps.Directives := ctLeaveALone;

  // stay the same
  TestProcessResult(TCapitalisation, LOWER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, MIXED_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsUpper;
begin
  FormatSettings.Caps.Directives := ctUpper;

  TestProcessResult(TCapitalisation, LOWER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, UPPER_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsLower;
begin
  FormatSettings.Caps.Directives := ctLower;

  TestProcessResult(TCapitalisation, LOWER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, LOWER_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsMixed;
begin
  FormatSettings.Caps.Directives := ctMixed;

  TestProcessResult(TCapitalisation, LOWER_UNIT, MIXED_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, MIXED_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, MIXED_UNIT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestCapitalisation.Suite);
end.
