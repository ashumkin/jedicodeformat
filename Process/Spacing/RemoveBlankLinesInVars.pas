unit RemoveBlankLinesInVars;


{ AFS 9 March 2003
  At request, remove blank lines in procedure var declarations
    (and procedure const & type declarations)
}

interface

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TRemoveBlankLinesInVars = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;
  end;

implementation

uses FormatFlags, TokenType, WordMap, ParseTreeNodeType, TokenUtils;

{ TRemoveBlankLinesInVars }

constructor TRemoveBlankLinesInVars.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

procedure TRemoveBlankLinesInVars.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  lcTest: TSourceToken;
  liReturnCount: integer;
  liMaxReturns: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  if not InProcedureDeclarations(lcSourceToken) then
    exit;

  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn, ttComment]);

  if lcNext = nil then
    exit;

  { don't remove blank lines before the proc header,
    or before a contained fn or proc }
  if lcNext.Word in ProcedureWords then
    exit;

  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
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
