unit RemoveConsecutiveWhiteSpace;

{
 AFS 29 Dec 2002

  Visitor to remove consecutive whitespace
  Obfuscation
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveConsecutiveWhiteSpace, released May 2003.
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
  TRemoveConsecutiveWhiteSpace = class(TSwitchableVisitor)
  private
    fbWhiteSpaceLast: boolean;
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject;
      var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;


implementation

uses SourceToken, Tokens, FormatFlags, TokenUtils;

constructor TRemoveConsecutiveWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TRemoveConsecutiveWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { delete whitespace if the last one was also whitespace }
  if (lcSourceToken.TokenType = ttWhiteSpace) and fbWhiteSpaceLast then
    BlankToken(lcSourceToken);

  fbWhiteSpaceLast := (lcSourceToken.TokenType = ttWhiteSpace);
end;

end.
