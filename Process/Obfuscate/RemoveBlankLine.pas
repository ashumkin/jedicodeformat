unit RemoveBlankLine;

{ AFS 17 Jan 2003
  Obfuscate - remove blank lines }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveBlankLine, released May 2003.
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
------------------------------------------------------------------------------*)
{*)}

interface

uses SwitchableVisitor, VisitParseTree;

type
  TRemoveBlankLine = class(TSwitchableVisitor)
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;

implementation

uses
  SourceToken, Tokens, 
  FormatFlags, TokenUtils;


constructor TRemoveBlankLine.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TRemoveBlankLine.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  { find next, excluding spaces and comments, except '//' comment }
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
  while (lcNext <> nil) and (lcNext.TokenType = ttComment) and (lcNext.CommentStyle <> eDoubleSlash) do
    lcNext := lcNext.NextTokenWithExclusions([ttWhiteSpace]);

  {
    A return, followed by another return (with nothing of substance between them)
    is a blank line, so kill one of them
    thia pplies even in ASM blocks  }
  if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    BlankToken(lcSourceToken);

end;

end.