unit RemoveReturnsBeforeEnd;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveReturnsBeforeEnd, released May 2003.
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

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TRemoveReturnsBeforeEnd = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;
  end;

implementation

uses FormatFlags, TokenType, WordMap, ParseTreeNodeType, TokenUtils;

{ TRemoveReturnsBeforeEnd }

constructor TRemoveReturnsBeforeEnd.Create;
begin
  inherited;
end;

procedure TRemoveReturnsBeforeEnd.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  lcTest: TSourceToken;
  liReturnCount: integer;
  liMaxReturns: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  if not InStatements(lcSourceToken) then
    exit;

  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);

  if lcNext.Word <> wEnd then
    exit;

  liReturnCount := 0;
  liMaxReturns := 2;
  lcTest := lcSourceToken;

  { remove all returns up to that point (except one) }
  while (lcTest <> lcNext) do
  begin
    if (lcTest.TokenType = ttReturn) then
    begin
      // allow two returns -> 1 blank line
      inc(liReturnCount);
      if (liReturnCount > liMaxReturns) then
      begin
        lcTest.TokenType := ttWhiteSpace;
        lcTest.SourceCode := '';
      end;
    end;
    lcTest := lcTest.NextToken;
  end;
end;

end.