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
------------------------------------------------------------------------------*)

interface

uses
  Classes,
  TestFrameWork,
  StringsConverter, BaseTestProcess;

type
  TTestTextAfterUnitEnd = class(TBaseTestProcess)
  private
    fiSaveLines: integer;
  protected
    procedure Setup; override;
    procedure Teardown; override;

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

uses JclStrings,
  JcfSettings,
  ReturnsAfterFinalEnd;

{ TTestTextAfterUnitEnd }

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
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + AnsiLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestTooMany;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + AnsiLineBreak +  AnsiLineBreak + AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + AnsiLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestComment;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + '//foo' + AnsiLineBreak +  AnsiLineBreak + AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + '//foo' + AnsiLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestTextAfterUnitEnd.TestText;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + 'junk' + AnsiLineBreak +
    AnsiLineBreak + AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + 'junk' + AnsiLineBreak;
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestMoreText;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk' + AnsiLineBreak + AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestText3;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk' + AnsiLineBreak +
    'warrawak' + AnsiLineBreak +
    AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk' + AnsiLineBreak +
    'warrawak';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestTextAfterUnitEnd.TestText4;
const
  IN_UNIT_TEXT = TEST_UNIT  + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk' + AnsiLineBreak +
    'warrawak' + AnsiLineBreak +
    'narrank' + AnsiLineBreak +
    AnsiLineBreak;
  OUT_UNIT_TEXT = TEST_UNIT + AnsiLineBreak + 'junk' + AnsiLineBreak +
    'more junk' + AnsiLineBreak +
    'warrawak' + AnsiLineBreak +
    'narrank';
begin
  FormatSettings.Returns.NumReturnsAfterFinalEnd := 3;
  TestProcessResult(TReturnsAfterFinalEnd, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
 TestFramework.RegisterTest('Processes', TTestTextAfterUnitEnd.Suite);
end.
