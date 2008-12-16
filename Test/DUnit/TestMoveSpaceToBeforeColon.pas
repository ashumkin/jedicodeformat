unit TestMoveSpaceToBeforeColon;

(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestMoveSpaceToBeforeColon, released December 2008.
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

{ Test the "move spcae to before colon" process }

uses
  TestFrameWork,
  BaseTestProcess,
  SetTransform;

type
  TTestMoveSpaceToBeforeColon = class(TBaseTestProcess)
  published
    procedure TestLeft_Move;
    procedure TestMid_Move;
    procedure TestRight_Move;
  end;

implementation

uses
  MoveSpaceToBeforeCOlon,
  JcfStringUtils,
  JcfSettings;

const
  LEFT_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a:      integer;' + NativeLineBreak +
    '  bee:    boolean;' + NativeLineBreak +
    '  deedee: string;' + NativeLineBreak +
    UNIT_FOOTER;

  RIGHT_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a      :integer;' + NativeLineBreak +
    '  bee    :boolean;' + NativeLineBreak +
    '  deedee :string;' + NativeLineBreak +
    UNIT_FOOTER;

  MIXED_UNIT_TEXT = UNIT_HEADER +
    'var' + NativeLineBreak +
    '  a    :  integer;' + NativeLineBreak +
    '  bee:    boolean;' + NativeLineBreak +
    '  deedee :string;' + NativeLineBreak +
    UNIT_FOOTER;

procedure TTestMoveSpaceToBeforeColon.TestLeft_Move;
begin
  TestProcessResult(TMoveSpaceToBeforeColon, LEFT_UNIT_TEXT, RIGHT_UNIT_TEXT);
end;

procedure TTestMoveSpaceToBeforeColon.TestMid_Move;
begin
  TestProcessResult(TMoveSpaceToBeforeColon, MIXED_UNIT_TEXT, RIGHT_UNIT_TEXT);
end;

procedure TTestMoveSpaceToBeforeColon.TestRight_Move;
begin
  TestProcessResult(TMoveSpaceToBeforeColon, RIGHT_UNIT_TEXT, RIGHT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestMoveSpaceToBeforeColon.Suite);
end.
