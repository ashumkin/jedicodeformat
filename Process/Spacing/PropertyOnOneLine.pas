unit PropertyOnOneLine;

{ AFS 8 March 2003
  put a property all on one line 
}

interface

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TPropertyOnOneLine = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;
  end;

implementation

uses FormatFlags, TokenType, WordMap, ParseTreeNodeType;

constructor TPropertyOnOneLine.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn, eRemoveSpace];
end;

procedure TPropertyOnOneLine.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext, lcNext2, lcNextSolid: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  // never remove a return at the end of a comment like this one! ->
  if (lcSourceToken.TokenType = ttComment) and (lcSourceToken.CommentStyle = eDoubleSlash) then
    exit;

  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  { inpsect returns in property defs }
  if not (lcNext.TokenType = ttReturn) then
    exit;

  if not lcNext.HasParentNode(nProperty) then
    exit;

  { allow a return before the property starts :)  }
  lcNextSolid := lcNext.NextSolidToken;
  if lcNextSolid = nil then
    exit;
  if lcNextSolid.Word = wProperty then
    exit;


  while (lcNext <> nil) and (lcNext.TokenType in [ttReturn, ttWhiteSpace]) and
    lcNext.HasParentNode(nProperty) do
  begin
    // don't kill returns before a comment
    lcNext2 := lcNext.NextToken;
    if (lcNext2 <> nil) or (lcNext2.TokenType = ttComment) then
      break;

    if (lcNext.TokenType = ttReturn) then
    begin
      lcNext.TokenType := ttWhiteSpace;
      lcNext.SourceCode := '';
    end
    else if lcNext.TokenType = ttWhiteSpace then
    begin
      lcNext.SourceCode := ' '; //reduce to a single space
    end;

    lcNext := lcNext2;
  end;

end;

end.
