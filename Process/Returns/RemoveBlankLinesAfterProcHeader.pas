unit RemoveBlankLinesAfterProcHeader;

interface

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TRemoveBlankLinesAfterProcHeader = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;
  end;

implementation

uses FormatFlags, TokenType, WordMap, ParseTreeNodeType;

function IsPlaceForBlankLineRemoval(const ptToken, ptNextSolidToken: TSourceToken): boolean;
begin
  Result := False;

  if (ptToken = nil) or (ptNextSolidToken = nil) then
    exit;

  { assume we're already under a procedure decl as tested below }

  { before the begin }
  if ptToken.HasParentNode(nCompoundStatement) and (ptNextSolidToken.Word = wBegin) then
  begin
    Result := True;
    exit;
  end;

  { before the type, const, lable, val }
  if ptToken.HasParentNode(nDeclSection) and
    (ptNextSolidToken.Word in [wType, wVar, wConst, wLabel]) then
  begin
    Result := True;
    exit;
  end;
end;

{ TRemoveBlankLinesAfterProcHeader }

constructor TRemoveBlankLinesAfterProcHeader.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

procedure TRemoveBlankLinesAfterProcHeader.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext, lcTest: TSourceToken;
  liReturnCount: integer;
  liMaxReturns: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  { must be in procedure declarations or directives}
  if not lcSourceToken.HasParentNode(ProcedureNodes) then
    exit;


  if lcSourceToken.TokenType <> ttReturn then
    exit;

  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);

  { it must be a 'var', 'const', 'type' or 'begin'
   in the procedure defs/body section }


  { can be type, var etc. var}
  if not IsPlaceForBlankLineRemoval(lcSourceToken, lcNext) then
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
