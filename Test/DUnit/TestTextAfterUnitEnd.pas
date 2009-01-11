unit TestTextAfterUnitEnd;

{ AFS Noc 2003
  Test text after unit end  }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestTextAfterUnitEnd, released nov 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
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
  BaseTestProcess;

type
  TTestTextAfterUnitEnd = class(TBaseTestProcess)
  private
    fiSaveLines: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestTooFew;
    procedure TestTooMany;

    procedure TestComment;
    procedure TestText;
    procedure TestMoreText;
    procedure TestText3;
    procedure TestText4;

  end;

implementation

uses
  JcfStringUtils,
  JcfSettings,
  ReturnsAfterFinalEnd;

const
  TEST_UNIT = UNIT_HEADER + UNIT_FOOTER;

procedure TTestTextAfterUnitEnd.Setup;
begin
  inherited;
  fiSaveLines := FormatSettings.Returns.NumReturnsAfterFinalEnd;
end;

procedure TTestTextAfterUnitEnd.Teardown;
begin
  inherited;
  FormatSettings.Returns.NumReturnsAfterFinalEnd := fiSaveLines;
end;

procedure TTestTextAfterUnitEnd.TestTooFew;
const
  IN_UNIT_TEXT = TEST_UNIT;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + NativeLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestTooMany;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + NativeLineBreak +  NativeLineBreak + NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + NativeLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestComment;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + '//foo' + NativeLineBreak +  NativeLineBreak + NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + '//foo' + NativeLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestTextAfterUnitEnd.TestText;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + 'junk' + NativeLineBreak +
    NativeLineBreak + NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + 'junk' + NativeLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestMoreText;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk' + NativeLineBreak + NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestText3;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk' + NativeLineBreak +
    'warrawak' + NativeLineBreak +
    NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk' + NativeLineBreak +
    'warrawak';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestText4;
const
  IN_UNIT_TEXT = TEST_UNIT  + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk' + NativeLineBreak +
    'warrawak' + NativeLineBreak +
    'narrank' + NativeLineBreak +
    NativeLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + NativeLineBreak + 'junk' + NativeLineBreak +
    'more junk' + NativeLineBreak +
    'warrawak' + NativeLineBreak +
    'narrank';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestTextAfterUnitEnd.Suite);
end.
