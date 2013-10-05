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

  lcSetCaps := JcfFormatSettings.Caps;

  fbSaveEnabled := lcSetCaps.Enabled;
  feSaveReservedWords := lcSetCaps.ReservedWords;
  feSaveOperators := lcSetCaps.Operators;
  feSaveDirectives := lcSetCaps.Directives;
  feSaveConstants := lcSetCaps.Constants;
  feSaveTypes := lcSetCaps.Types;

  fbSaveAnyWordCapsEnabled := JcfFormatSettings.SpecificWordCaps.Enabled;

  fSaveIdentifierCaps := JcfFormatSettings.IdentifierCaps.Enabled;
  fSaveNotIdentifierCaps := JcfFormatSettings.NotIdentifierCaps.Enabled;

  fSaveIdentifierCapsWords := TStringList.Create;
  fSaveIdentifierCapsWords.Assign(JcfFormatSettings.IdentifierCaps.Words);
  fSaveNotIdentifierCapsWords := TStringList.Create;
  fSaveNotIdentifierCapsWords.Assign(JcfFormatSettings.NotIdentifierCaps.Words);


  // default setup
 JcfFormatSettings.Caps.Enabled := True;
 JcfFormatSettings.SpecificWordCaps.Enabled := False;
end;

procedure TTestCapitalisation.Teardown;
var
  lcSetCaps: TSetCaps;
begin
  inherited;

  lcSetCaps := JcfFormatSettings.Caps;

  lcSetCaps.Enabled := fbSaveEnabled;
  lcSetCaps.ReservedWords := feSaveReservedWords;
  lcSetCaps.Operators := feSaveOperators;
  lcSetCaps.Directives := feSaveDirectives;
  lcSetCaps.Constants := feSaveConstants;
  lcSetCaps.Types := feSaveTypes;

 JcfFormatSettings.IdentifierCaps.Enabled := fSaveIdentifierCaps;
 JcfFormatSettings.NotIdentifierCaps.Enabled := fSaveNotIdentifierCaps;

 JcfFormatSettings.IdentifierCaps.Words.Assign(fSaveIdentifierCapsWords);
  FreeAndNil(fSaveIdentifierCapsWords);
 JcfFormatSettings.NotIdentifierCaps.Words.Assign(fSaveNotIdentifierCapsWords);
  FreeAndNil(fSaveNotIdentifierCapsWords);

 JcfFormatSettings.SpecificWordCaps.Enabled := fbSaveAnyWordCapsEnabled;
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
 JcfFormatSettings.Caps.Directives := ctLeaveALone;

  // stay the same
  TestProcessResult(TCapitalisation, LOWER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, MIXED_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsUpper;
begin
 JcfFormatSettings.Caps.Directives := ctUpper;

  TestProcessResult(TCapitalisation, LOWER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, UPPER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, UPPER_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsLower;
begin
 JcfFormatSettings.Caps.Directives := ctLower;

  TestProcessResult(TCapitalisation, LOWER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, UPPER_UNIT, LOWER_UNIT);
  TestProcessResult(TCapitalisation, MIXED_UNIT, LOWER_UNIT);
end;

procedure TTestCapitalisation.TestPropertyCapsMixed;
begin
 JcfFormatSettings.Caps.Directives := ctMixed;

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
 JcfFormatSettings.IdentifierCaps.Enabled := False;
 JcfFormatSettings.IdentifierCaps.Words.Clear;
 JcfFormatSettings.NotIdentifierCaps.Enabled := False;
 JcfFormatSettings.NotIdentifierCaps.Words.Clear;

  // no change
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_IN);

 JcfFormatSettings.IdentifierCaps.Enabled := True;
 JcfFormatSettings.NotIdentifierCaps.Enabled := True;

  // no change
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_IN);

 JcfFormatSettings.IdentifierCaps.Add('Read');
 JcfFormatSettings.IdentifierCaps.Add('Write');
 JcfFormatSettings.IdentifierCaps.Add('Public');
 JcfFormatSettings.IdentifierCaps.Add('Strict');

  // identifiers capitalised
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_OUT_IDS);

 JcfFormatSettings.NotIdentifierCaps.Add('READ');
 JcfFormatSettings.NotIdentifierCaps.Add('WRITE');
 JcfFormatSettings.NotIdentifierCaps.Add('PUBLIC');

  // both identifers and reserved words
  TestProcessResult(TIdentifierCaps, TEST_IDENTIFIER_CAPS_IN, TEST_IDENTIFIER_CAPS_OUT_BOTH);
end;



initialization
  TestFramework.RegisterTest('Processes', TTestCapitalisation.Suite);
end.
