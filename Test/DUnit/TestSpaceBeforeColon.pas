unit TestSpaceBeforeColon;

{ AFS October 2008
  Test the option for spacing before colon
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestSpaceBeforeColon, released October 2008.
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
  TTestSpaceBeforeColon = class(TBaseTestProcess)
  private
    fiSaveMaxSpaces: integer;
    feSaveOperatorSetting: TTriOptionStyle;

    fiSaveSpacesBeforeColonVar: integer;
    fiSaveSpacesBeforeColonConst: integer;
    fiSaveSpacesBeforeColonParam: integer;
    fiSaveSpacesBeforeColonFn: integer;
    fiSaveSpacesBeforeColonClassVar: integer;
    fiSaveSpacesBeforeColonRecordField: integer;
    fiSaveSpacesBeforeColonCaseLabel: integer;
    fiSaveSpacesBeforeColonLabel: integer;
    fiSaveSpacesBeforeColonInGeneric: integer;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestNoSpaceBeforeColonFnRemove;
    procedure TestNoSpaceBeforeColonFnSame;

    procedure TestSingleSpaceBeforeColonFnAdd;
    procedure TestSingleSpaceBeforeColonFnSame;

    procedure TestNoSpaceBeforeColonVarRemove;
    procedure TestNoSpaceBeforeColonVarSame;
    procedure TestSingleSpaceBeforeColonVarAdd;
    procedure TestSingleSpaceBeforeColonVarSame;

    procedure TestNoSpaceBeforeColonConstRemove;
    procedure TestNoSpaceBeforeColonConstSame;
    procedure TestSingleSpaceBeforeColonConstAdd;
    procedure TestSingleSpaceBeforeColonConstSame;
  end;

implementation

uses JclAnsiStrings,
  JcfSettings,
  SpaceBeforeColon;

procedure TTestSpaceBeforeColon.Setup;
begin
  inherited;

  fiSaveMaxSpaces := JcfFormatSettings.Spaces.MaxSpacesInCode;
  feSaveOperatorSetting := JcfFormatSettings.Spaces.SpaceForOperator;

  fiSaveSpacesBeforeColonVar := JcfFormatSettings.Spaces.SpacesBeforeColonVar;
  fiSaveSpacesBeforeColonConst := JcfFormatSettings.Spaces.SpacesBeforeColonConst;
  fiSaveSpacesBeforeColonParam := JcfFormatSettings.Spaces.SpacesBeforeColonParam;
  fiSaveSpacesBeforeColonFn := JcfFormatSettings.Spaces.SpacesBeforeColonFn;
  fiSaveSpacesBeforeColonClassVar := JcfFormatSettings.Spaces.SpacesBeforeColonClassVar;
  fiSaveSpacesBeforeColonRecordField := JcfFormatSettings.Spaces.SpacesBeforeColonRecordField;
  fiSaveSpacesBeforeColonCaseLabel := JcfFormatSettings.Spaces.SpacesBeforeColonCaseLabel;
  fiSaveSpacesBeforeColonLabel := JcfFormatSettings.Spaces.SpacesBeforeColonLabel;
  fiSaveSpacesBeforeColonInGeneric := JcfFormatSettings.Spaces.SpacesBeforeColonInGeneric;
end;

procedure TTestSpaceBeforeColon.Teardown;
begin
  inherited;

 JcfFormatSettings.Spaces.MaxSpacesInCode := fiSaveMaxSpaces;
 JcfFormatSettings.Spaces.SpaceForOperator := feSaveOperatorSetting;

 JcfFormatSettings.Spaces.SpacesBeforeColonVar := fiSaveSpacesBeforeColonVar;
 JcfFormatSettings.Spaces.SpacesBeforeColonConst := fiSaveSpacesBeforeColonConst;
 JcfFormatSettings.Spaces.SpacesBeforeColonParam := fiSaveSpacesBeforeColonParam;
 JcfFormatSettings.Spaces.SpacesBeforeColonFn := fiSaveSpacesBeforeColonFn;
 JcfFormatSettings.Spaces.SpacesBeforeColonClassVar := fiSaveSpacesBeforeColonClassVar;
 JcfFormatSettings.Spaces.SpacesBeforeColonRecordField := fiSaveSpacesBeforeColonRecordField;
 JcfFormatSettings.Spaces.SpacesBeforeColonCaseLabel := fiSaveSpacesBeforeColonCaseLabel;
 JcfFormatSettings.Spaces.SpacesBeforeColonLabel := fiSaveSpacesBeforeColonLabel;
 JcfFormatSettings.Spaces.SpacesBeforeColonInGeneric := fiSaveSpacesBeforeColonInGeneric;
end;


procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonFnRemove;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' function foo : integer; begin result := 2; end; ' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonFn := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonFnSame;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonFn := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonFnAdd;
const
  //JcfSettings.SetSpaces.SpacesBeforeColonFn := 0;
  IN_UNIT_TEXT  = UNIT_HEADER + ' function foo: integer; begin result := 2; end; ' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo : integer; begin result := 2; end; ' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonFn := 1;
  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonFnSame;
const
  //JcfSettings.SetSpaces.SpacesBeforeColonFn := 0;
  IN_UNIT_TEXT  = UNIT_HEADER + ' function foo : integer; begin result := 2; end; ' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' function foo : integer; begin result := 2; end; ' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonFn := 1;
  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonVarRemove;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' var foo : integer;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' var foo: integer;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonVar := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonVarSame;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' var foo: integer;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' var foo: integer;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonVar := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonVarAdd;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' var foo: integer;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' var foo : integer;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonVar := 1;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonVarSame;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' var foo : integer;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' var foo : integer;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonVar := 1;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonConstRemove;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' const foo : integer = 3;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' const foo: integer = 3;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonConst := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestNoSpaceBeforeColonConstSame;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' const foo: integer = 3;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' const foo: integer = 3;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonConst := 0;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonConstAdd;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' const foo: integer = 3;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' const foo : integer = 3;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonConst := 1;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestSpaceBeforeColon.TestSingleSpaceBeforeColonConstSame;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' const foo : integer = 3;' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' const foo : integer = 3;' +
    UNIT_FOOTER;
begin
 JcfFormatSettings.Spaces.SpacesBeforeColonConst := 1;

  TestProcessResult(TSpaceBeforeColon, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestSpaceBeforeColon.Suite);
end.
