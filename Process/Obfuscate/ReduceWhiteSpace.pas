unit ReduceWhiteSpace;

{ AFS 28 Dec 2002

  Visitor to reduce all whitespace to single spaces
  Obfuscation
}

interface

uses BaseVisitor, VisitParseTree;

type
  TReduceWhiteSpace = class(TBaseTreeNodeVisitor)
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses SourceToken, TokenType;

procedure TReduceWhiteSpace.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttWhiteSpace then
    lcSourceToken.SourceCode := ' ';

end;

end.
