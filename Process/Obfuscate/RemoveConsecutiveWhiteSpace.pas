unit RemoveConsecutiveWhiteSpace;

{
 AFS 29 Dec 2002

  Visitor to remove consecutive whitespace
  Obfuscation
}

interface

uses BaseVisitor, VisitParseTree;

type
  TRemoveConsecutiveWhiteSpace = class(TBaseTreeNodeVisitor)
    private
      fbWhiteSpaceLast: boolean;
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses SourceToken, TokenType;

procedure TRemoveConsecutiveWhiteSpace.VisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { delete whitespace if the last one was also whitespace }
  if (lcSourceToken.TokenType = ttWhiteSpace) and fbWhiteSpaceLast then
    prVisitResult.action := aDelete;

  fbWhiteSpaceLast := (lcSourceToken.TokenType = ttWhiteSpace);
end;

end.

