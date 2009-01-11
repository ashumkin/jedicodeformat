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

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  Classes,
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

    fSaveIdentifierCaps: boolean;
    fSaveNotIdentifierCaps: boolean;
    fSaveIdentifierCapsWords: TStringList;
    fSaveNotIdentifierCapsWords: TStringList;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestPropertyCapsNull;
    procedure TestPropertyCapsUpper;
    procedure TestPropertyCapsMixed;
    procedure TestPropertyCapsLower;

    procedure TestIdentifierCaps;
 end;

implementation

uses
  SysUtils,
  { local }
  JcfStringUtils,
  Capitalisation, JcfSettings, SetCaps, IdentifierCaps;


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

  fSaveIdentifierCaps := FormatSettings.IdentifierCaps.Enabled;
  fSaveNotIdentifierCaps := FormatSettings.NotIdentifierCaps.Enabled;

  fSaveIdentifierCapsWords := TStringList.Create;
  fSaveIdentifierCapsWords.Assign(FormatSettings.IdentifierCaps.Words);
  fSaveNotIdentifierCapsWords := TStringList.Create;
  fSaveNotIdentifierCapsWords.Assign(FormatSettings.NotIdentifierCaps.Words);


  // default setup
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

  FormatSettings.IdentifierCaps.Enabled := fSaveIdentifierCaps;
  FormatSettings.NotIdentifierCaps.Enabled := fSaveNotIdentifierCaps;

  FormatSettings.IdentifierCaps.Words.Assign(fSaveIdentifierCapsWords);
  FreeAndNil(fSaveIdentifierCapsWords);
  FormatSettings.NotIdentifierCaps.Words.Assign(fSaveNotIdentifierCapsWords);
  FreeAndNil(fSaveNotIdentifierCapsWords);

  FormatSettings.SpecificWordCaps.Enabled := fbSaveAnyWordCapsEnabled;
end;


const
  LOWER_PROPERTY =
    'private' + NativeLineBreak +
    'fiFoo: integer;' + NativeLineBreak +
    'public' + NativeLineBreak +
    ' property foo read fiFoo write fiFoo;' + NativeLineBreak;

  UPPER_PROPERTY =
    'PRIVATE' + NativeLineBreak +
    'fiFoo: integer;' + NativeLineBreak +
    'PUBLIC' + NativeLineBreak +
      ' property foo READ fiFoo WRITE fiFoo;' + NativeLineBreak;

  MIXED_PROPERTY =
    'Private' + NativeLineBreak +
    'fiFoo: integer;' + NativeLineBreak +
    'Public' + NativeLineBreak +
      ' property foo Read fiFoo Write fiFoo;' + NativeLineBreak;

  UNIT_TEXT_PREFIX  = UNIT_HEADER +
    'type TFoo = class' + NativeLineBreak;

  UNIT_TEXT_SUFFIX = 'end;' + NativeLineBreak +
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

const
  TEST_IDENTIFIER_CAPS_IN =
    'unit testCaps;' + NativeLineBreak +
    'interface' + NativeLineBreak +
    'type' + NativeLineBreak +
    '  TTester = class' + NativeLineBreak +
    '  private' + NativeLineBreak +
    '    fbRead: boolean;' + NativeLineBreak +
    '    fbWrite: boolean;' + NativeLineBreak +
    '    procedure SetRead(const Value: boolean);' + NativeLineBreak +
    '    public' + NativeLineBreak +
    '      property read: boolean read fbRead write SetRead;' + NativeLineBreak +
    '      property write: boolean read fbWrite write fbWrite;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'procedure TTester.SetRead(const Value: boolean);' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  strict: integer;' + NativeLineBreak +
    '  public: boolean;' + NativeLineBreak +
    '  override: boolean;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  fbRead := Value;' + NativeLineBreak +
    'end;' + NativeLineBreak +
    'end.';

  TEST_IDENTIFIER_CAPS_OUT_IDS =
    'unit testCaps;' + NativeLineBreak +
    'interface' + NativeLineBreak +
    'type' + NativeLineBreak +
    '  TTester = class' + NativeLineBreak +
    '  private' + NativeLineBreak +
    '    fbRead: boolean;' + NativeLineBreak +
    '    fbWrite: boolean;' + NativeLineBreak +
    '    procedure SetRead(const Value: boolean);' + NativeLineBreak +
    '    public' + NativeLineBreak +
    '      property Read: boolean read fbRead write SetRead;' + NativeLineBreak +
    '      property Write: boolean read fbWrite write fbWrite;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'procedure TTester.SetRead(const Value: boolean);' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  Strict: integer;' + NativeLineBreak +
    '  Public: boolean;' + NativeLineBreak +
    '  override: boolean;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  fbRead := Value;' + NativeLineBreak +
    'end;' + NativeLineBreak +
    'end.';

    TEST_IDENTIFIER_CAPS_OUT_BOTH =
    'unit testCaps;' + NativeLineBreak +
    'interface' + NativeLineBreak +
    'type' + NativeLineBreak +
    '  TTester = class' + NativeLineBreak +
    '  private' + NativeLineBreak +
    '    fbRead: boolean;' + NativeLineBreak +
    '    fbWrite: boolean;' + NativeLineBreak +
    '    procedure SetRead(const Value: boolean);' + NativeLineBreak +
    '    PUBLIC' + NativeLineBreak +
    '      property Read: boolean READ fbRead WRITE SetRead;' + NativeLineBreak +
    '      property Write: boolean READ fbWrite WRITE fbWrite;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'implementation' + NativeLineBreak +
    'procedure TTester.SetRead(const Value: boolean);' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  Strict: integer;' + NativeLineBreak +
    '  Public: boolean;' + NativeLineBreak +
    '  override: boolean;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  fbRead := Value;' + NativeLineBreak +
    'end;' + NativeLineBreak +
    'end.';



procedure TTestCapitalisation.TestIdentifierCaps;
begin
  FormatSettings.IdentifierCaps.Enabled := False;
  FormatSettings.IdentifierCaps.Words.Clear;
  FormatSettings.NotIdentifierCaps.Enabled := False;
  FormatSettings.NotIdentifierCaps.Words.Clear;

  // no change
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_IN);

  FormatSettings.IdentifierCaps.Enabled := True;
  FormatSettings.NotIdentifierCaps.Enabled := True;

  // no change
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_IN);

  FormatSettings.IdentifierCaps.Add('Read');
  FormatSettings.IdentifierCaps.Add('Write');
  FormatSettings.IdentifierCaps.Add('Public');
  FormatSettings.IdentifierCaps.Add('Strict');

  // identifiers capitalised
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_OUT_IDS);

  FormatSettings.NotIdentifierCaps.Add('READ');
  FormatSettings.NotIdentifierCaps.Add('WRITE');
  FormatSettings.NotIdentifierCaps.Add('PUBLIC');

  // both identifers and reserved words
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_OUT_BOTH);
end;



initialization
  TestFramework.RegisterTest('Processes', TTestCapitalisation.Suite);
end.
