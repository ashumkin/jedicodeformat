unit NoSpaceBefore;

{ AFS 5 Jan 2002
  No space before certain tokens (e.g. '.' ';'
  the Colon has it's own unit }


interface

uses SwitchableVisitor, VisitParseTree;

type
  TNoSpaceBefore = class(TSwitchableVisitor)
    private
      fbSafeToRemoveReturn: boolean;  // this taken from NoReturnBefore
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;


implementation

uses SourceToken, TokenType, WordMap, ParseTreeNodeType, FormatFlags,
  TokenUtils;

const
  NoSpaceAnywhere: TTokenTypeSet = [ttSemiColon, ttDot, ttComma,
    ttCloseSquareBracket, ttCloseBracket];

function HasNoSpaceBefore(const pt: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  // '@@' in asm, e.g. "JE @@initTls" needs the space
  if pt.HasParentNode(nAsm) then
    exit;

  if pt.TokenType in NoSpaceAnywhere then
  begin
    Result := True;
    exit;
  end;

  { hat (dereference) in expression is unary postfix operator - so no space before it }
  if (pt.HasParentNode(nExpression)) and (pt.word = wHat) then
  begin
    Result := True;
    exit;
  end;

  { no space before open brackets for fn name - declaration or use }
  if IsActualParamOpenBracket(pt) or IsFormalParamOpenBracket(pt) then
  begin
    Result := True;
    exit;
  end;

  { no space before colon -- anywhere? }
  if pt.TokenType = ttColon then
  begin
    Result := True;
    exit;
  end;

end;

constructor TNoSpaceBefore.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

procedure TNoSpaceBefore.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
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
  begin
    // the space
    prVisitResult.Action := aDelete;
  end;
end;

end.
