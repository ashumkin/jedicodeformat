unit VisitStripEmptySpace;

{ AFS 7 March 2003
  needed after SpaceBeforeColon
  It is possible that some tokens will be spaces
  with sourcecode = '' null/empty string
  discared these
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is VisitStripEmptySpace, released May 2003.
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

uses BaseVisitor, VisitParseTree, FormatFlags;

type
  TVisitStripEmptySpace = class(TBaseTreeNodeVisitor)
  public
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses SourceToken, Tokens;

procedure TVisitStripEmptySpace.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcToken);

  if (lcSourceToken <> nil) and (lcSourceToken.TokenType = ttWhiteSpace) and (lcSourceToken.SourceCode = '') then
    prVisitResult.Action := aDelete;
end;

end.