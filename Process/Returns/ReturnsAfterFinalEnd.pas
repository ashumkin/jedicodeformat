unit ReturnsAfterFinalEnd;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReturnsAfterFinalEnd.pas, released September 2003.
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

{ AFS 27 Sept 2003
  process to standardise the number of returns
  after the final "end." of the unit }

uses SwitchableVisitor, VisitParseTree;

type
  TReturnsAfterFinalEnd = class(TSwitchableVisitor)
  private
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

  end;


implementation

uses
  JcfSettings,
  SourceToken, FormatFlags, Tokens, ParseTreeNodeType, TokenUtils;

{ TReturnsAfterFinalEnd }

constructor TReturnsAfterFinalEnd.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddReturn, eRemoveReturn];
end;

procedure TReturnsAfterFinalEnd.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcCurrent: TSourceToken;
  liReturnsWanted, liReturnsFound: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType = ttDot) and
    (lcSourceToken.HasParentNode(TopOfFileSection, 1)) then
  begin
    // count the returns
    lcCurrent := lcSourceToken;
    liReturnsWanted := FormatSettings.Returns.NumReturnsAfterFinalEnd;
    liReturnsFound := 0;

    while lcCurrent <> nil do
    begin
      if lcCurrent.TokenType = ttReturn then
        inc(liReturnsFound);

      if liReturnsFound > liReturnsWanted then
      begin
        { this returns is surplus - remove it }
        lcCurrent.TokenType := ttWhiteSpace;
        lcCurrent.SourceCode := '';
      end;

      lcCurrent := lcCurrent.NextToken;
    end;

    { need to insert some returns? }
    while liReturnsFound < liReturnsWanted do
    begin
      InsertReturnAfter(lcSourceToken);
      inc(liReturnsFound);
    end;
  end;
end;

end.
