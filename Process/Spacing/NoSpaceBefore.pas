unit NoSpaceBefore;

{ AFS 5 Jan 2002
  No space before certain tokens (e.g. '.' ';'
  the Colon has it's own unit }


interface

uses BaseVisitor, VisitParseTree;


type
  TNoSpaceBefore = class(TBaseTreeNodeVisitor)
    private
      fbSafeToRemoveReturn: boolean;  // this taken from NoReturnBefore

    public
      constructor Create; override;

      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses SourceToken, TokenType, WordMap, ParseTreeNodeType;

const
  NoSpaceAnywhere: TTokenTypeSet = [ttSemiColon, ttDot, ttComma,
    ttCloseSquareBracket, ttCloseBracket];

function HasNoSpaceBefore(const pt: TSourceToken): boolean;
begin
  Result := False;

  // '@@' in asm, e.g. "JE @@initTls" needs the space

  if (pt.Word = wAtSign) and pt.HasParentNode(nAsmStatement) then
    exit;

  if pt.TokenType in NoSpaceAnywhere then
  begin
    Result := True;
    exit;
  end;

  { hat (dereference) in expression is unary postfix operator - so no space before it }
  if (pt.HasParentNode(nExpression)) and (pt.word = wHat) then
    Result := True;
end;

constructor TNoSpaceBefore.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
end;

procedure TNoSpaceBefore.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  // not safe to remove return at a comment like this
  if (lcSourceToken.TokenType = ttComment) and (lcSourceToken.CommentStyle = eDoubleSlash) then
    fbSafeToRemoveReturn := False
  else if (lcSourceToken.TokenType <> ttReturn) then
    fbSafeToRemoveReturn := True;

  // work on whitespace and returns
  if (not (lcSourceToken.TokenType in [ttWhiteSpace, ttReturn])) or (not fbSafeToRemoveReturn) then
    exit;

  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  if HasNoSpaceBefore(lcNext) then
    prVisitResult.Action := aDelete;
end;

end.
