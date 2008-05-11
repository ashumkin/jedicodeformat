unit TestFindReplace;


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestFindReplace, released Sept 2003.
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

{ test the find-replace feature }

{$I JcfGlobal.inc}

interface

uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestFindReplace = class(TBaseTestProcess)
  private

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoAction;
    procedure TestProc;
    procedure TestVar;
    procedure TestType;
  end;

implementation

uses
  SysUtils,
  JcfRegistrySettings, JcfSettings, TestConstants, FindReplace;

procedure TTestFindReplace.Setup;
var
  lsSettingsFileName: string;
begin
  inherited;

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { use clarify test settings }
  lsSettingsFileName := GetTestSettingsFileName;
  Check(FileExists(lsSettingsFileName), 'Settings file ' + lsSettingsFileName +
    ' not found');

  GetRegSettings.FormatConfigFileName := lsSettingsFileName;
  FormatSettings; // create and read
  FormatSettings.Obfuscate.Enabled := False;

  FormatSettings.Replace.Enabled := True;
  FormatSettings.Replace.Words.Clear;
  FormatSettings.Replace.Words.Add('Foo;Bar');

  FormatSettings.Replace.SplitWords
end;

procedure TTestFindReplace.TearDown;
begin
  FormatSettings.Replace.Enabled := False;
  FormatSettings.Replace.Words.Clear;
end;

procedure TTestFindReplace.TestNoAction;
const
  IN_UNIT_TEXT  = UNIT_HEADER + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
begin
  TestProcessResult(TFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestFindReplace.TestProc;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' procedure Foo; begin end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure Bar; begin end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestFindReplace.TestVar;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' procedure Foo2; var Foo: integer; begin end; ' +
    UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure Foo2; var Bar: integer; begin end; ' +
    UNIT_FOOTER;
begin
  TestProcessResult(TFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestFindReplace.TestType;
const
  IN_UNIT_TEXT  = UNIT_HEADER + ' procedure Foo2; var a: Foo; begin end; ' + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + ' procedure Foo2; var a: Bar; begin end; ' + UNIT_FOOTER;
begin
  TestProcessResult(TFindReplace, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestFindReplace.Suite);
end.
