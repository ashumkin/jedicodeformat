unit RemoveSpaceAtLineEnd;

{ AFS 10 May 2003
  remove trainling spaces on lines
  makes test fail, false delta }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveSpaceAtLineEnd, released May 2003.
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
  TRemoveSpaceAtLineEnd = class(TSwitchableVisitor)
    private
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;



implementation

uses  FormatFlags, SourceToken, TokenType;


constructor TRemoveSpaceAtLineEnd.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

procedure TRemoveSpaceAtLineEnd.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { is this white space? }
  if lcSourceToken.TokenType = ttWhiteSpace then
  begin
    { is a return next ? }
    lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
    if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    begin
      lcSourceToken.SourceCode := '';
    end;
  end;

end;

end.