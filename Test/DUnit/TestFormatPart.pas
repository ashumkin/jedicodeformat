unit TestFormatPart;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestFormatPart
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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
  BaseTestProcess, SettingsTypes;

type
  TTestFormatPart = class(TBaseTestProcess)
  private

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

uses
  JcfStringUtils,
  BlockStyles, JcfSettings, SetReturns;

const
  UNIT_IN = UNIT_HEADER +
    'procedure fred; ' + NativeLineBreak +
    'begin' + NativeLineBreak +
    'end;'  + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_OUT_TOP = SPACED_UNIT_HEADER +
    'procedure fred; ' + NativeLineBreak +
    'begin' + NativeLineBreak +
    'end;'  + NativeLineBreak +
    UNIT_FOOTER;

  UNIT_OUT_TOP_MID = SPACED_UNIT_HEADER +  NativeLineBreak +
    'procedure fred; ' + NativeLineBreak +
    'begin' + NativeLineBreak +
    'end;'  + NativeLineBreak +
    UNIT_FOOTER;


  UNIT_OUT_MID = UNIT_HEADER + 
    'procedure fred; ' + NativeLineBreak +
    'begin' + NativeLineBreak +
    'end;'  + NativeLineBreak +
    UNIT_FOOTER;

procedure TTestFormatPart.Setup;
begin
  inherited;

end;

procedure TTestFormatPart.Teardown;
begin
  inherited;

end;

procedure TTestFormatPart.Test1;
begin
  TestFormatPartResult(UNIT_IN, UNIT_OUT_TOP, 10, 20);
end;

procedure TTestFormatPart.Test2;
begin
  TestFormatPartResult(UNIT_IN, UNIT_OUT_TOP_MID, 20, 30);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestFormatPart.Suite);
end.
