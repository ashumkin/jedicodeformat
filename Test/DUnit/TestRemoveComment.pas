unit TestRemoveComment;

{ AFS 10 Nov 2003
  Test comment removal }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestRemoveComment, released November 2003.
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
{*)}

interface

uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestRemoveComment = class(TBaseTestProcess)
  private
  public
    procedure Setup; override;
  published
    procedure TestNone;
    procedure TestBraces;
    procedure TestSlash;
    procedure TestBoth;
  end;

implementation

uses
  JclStrings,
  JcfSettings, RemoveComment;

procedure TTestRemoveComment.Setup;
begin
  inherited;
  FormatSettings.Comments.RemoveEmptyDoubleSlashComments := True;
  FormatSettings.Comments.RemoveEmptyCurlyBraceComments  := True;
end;


procedure TTestRemoveComment.TestNone;
const
  IN_UNIT_TEXT  = UNIT_HEADER + UNIT_FOOTER;
  OUT_UNIT_TEXT = UNIT_HEADER + UNIT_FOOTER;
begin
  TestProcessResult(TRemoveComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

procedure TTestRemoveComment.TestBraces;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    '{  }' +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    ' ' +
    UNIT_FOOTER;
begin
  TestProcessResult(TRemoveComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;


procedure TTestRemoveComment.TestSlash;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    '//  ' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    AnsiLineBreak +
    UNIT_FOOTER;
begin

end;

procedure TTestRemoveComment.TestBoth;
const
  IN_UNIT_TEXT = UNIT_HEADER +
    '//  ' + AnsiLineBreak +
    '{  }' + AnsiLineBreak +
    UNIT_FOOTER;

  OUT_UNIT_TEXT = UNIT_HEADER +
    ' ' + AnsiLineBreak +
    ' ' + AnsiLineBreak +
    UNIT_FOOTER;
begin
  TestProcessResult(TRemoveComment, IN_UNIT_TEXT, OUT_UNIT_TEXT);
end;

initialization
  TestFramework.RegisterTest('Processes', TTestRemoveComment.Suite);
end.
