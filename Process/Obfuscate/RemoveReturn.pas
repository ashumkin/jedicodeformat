unit RemoveReturn;

interface

uses BaseVisitor, VisitParseTree;

type
  TRemoveReturn = class(TBaseTreeNodeVisitor)
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;



implementation

uses ParseTreeNode, SourceToken, TokenType, ParseTreeNodeType;

procedure TRemoveReturn.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcPrev: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  // only act on returns
  if lcSourceToken.TokenType <> ttReturn then
    exit;

  { not in asm }
  if lcSourceToken.HasParentNode(nAsm) then
    exit;

  // never remove the return after a comment like this
  lcPrev := lcSourceToken.PriorTokenWithExclusions([ttWhiteSpace]);

  if (lcPrev <> nil) and (lcPrev.TokenType = ttComment) and (lcPrev.CommentStyle = eDoubleSlash) then
    exit;

  // transmute to white space  - may be needed as seperator
  lcSourceToken.SourceCode := ' ';
  lcSourceToken.TokenType := ttWhiteSpace;
end;

end.
