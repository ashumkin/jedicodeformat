unit RemoveComment;

{ AFS 28 Dec 2002

  Visitor to remove comments
  Obfuscation
}

interface

uses BaseVisitor, VisitParseTree;

type
  TRemoveComment = class(TBaseTreeNodeVisitor)
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses SourceToken, TokenType;

procedure TRemoveComment.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  (* turn comment to space - may be needed for token sep
    e.g. may be for a :=b{foo}to{bar}baz
  *)
  if lcSourceToken.TokenType = ttComment then
  begin
    lcSourceToken.TokenType := ttWhiteSpace;
    lcSourceToken.SourceCode := ' ';
  end;
end;

end.
