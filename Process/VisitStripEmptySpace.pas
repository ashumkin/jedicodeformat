unit VisitStripEmptySpace;

{ AFS 7 March 2003
  needed after SpaceBeforeColon
  It is possible that some tokens will be spaces
  with sourcecode = '' null/empty string
  discared these
}

interface

uses BaseVisitor, VisitParseTree, FormatFlags;

type
  TVisitStripEmptySpace = class(TBaseTreeNodeVisitor)
  public
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses SourceToken, TokenType;

procedure TVisitStripEmptySpace.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcToken);

  if (lcSourceToken <> nil) and (lcSourceToken.TokenType = ttWhiteSpace) and (lcSourceToken.SourceCode = '') then
    prVisitResult.Action := aDelete;
end;

end.
