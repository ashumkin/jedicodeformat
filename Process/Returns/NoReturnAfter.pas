unit NoReturnAfter;

{ AFS 11 Jan 2003
  Some tokens should not have a return after them for fomatting
}

interface

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TNoReturnAfter = class(TSwitchableVisitor)
    private
      fcLastSolidToken: TSourceToken;
      fbDoneWork: boolean;

      function NoDeclarationBefore: Boolean;
      function CommentBefore: Boolean;
      function NoSemiColonBefore: Boolean;

      function NeedsNoReturn(const pt: TSourceToken): boolean;

    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

    public
      constructor Create; override;

      property DoneWork: boolean read fbDoneWork;
  end;


implementation

uses WordMap, TokenType, ParseTreeNodeType, TokenUtils,
  SetReturns, JcfSettings, FormatFlags;


constructor TNoReturnAfter.Create;
begin
  inherited;
  fcLastSolidToken := nil;
  fbDoneWork := False;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

function TNoReturnAfter.NeedsNoReturn(const pt: TSourceToken): boolean;
const
  NoReturnWords: TWordSet = [wProcedure, wFunction,
    wConstructor, wDestructor, wProperty];
var
  lcSetReturns: TSetReturns;
  ptNext: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  lcSetReturns := FormatSettings.Returns;
  Assert(lcSetReturns <> nil);

  if FormatSettings.Returns.RemoveBadReturns then
  begin

    if pt.Word in NoReturnWords then
    begin
      Result := True;
      exit;
    end;

    { only place a return after a colon is legit is at a label
      in a proc body }
    if pt.TokenType = ttColon then
    begin
      if (not InStatements(pt)) and (RoundBracketLevel(pt) = 0) then
      begin
        Result := True;
        exit;
      end;
    end;

    { var x absolute y;  just after absolute is a bad place to break }
    if (pt.Word = wAbsolute) and pt.HasParentNode(nVarDecl) then
    begin
      Result := True;
      exit;
    end;

    { Default property values:
      No return after 'default' in property def on non-array property
      because it's always followed by a number, e.g.
      property FloonCount: integer default 12;
      as opposed to default (array) properties,
      eg property Items[piIndex: integer]; default; }
    if (pt.Word = wDefault) and pt.HasParentNode(nPropertySpecifier) then
    begin
      { use the persence of semicolon to distinguish
      Default property values from default (array) properties }
      if not SemiColonNext(pt) then
      begin
        Result := True;
        exit;
      end;
    end;

    { array property params - no returns }
    if (SquareBracketLevel(pt) > 0) and pt.HasParentNode(nPropertyParameterList) then
    begin
      { use the persence of semicolon to distinguish
      Default property values from default (array) properties }
      if not SemiColonNext(pt) then
      begin
        Result := True;
        exit;
      end;
    end;

    { in procedure params - no return after 'var' or 'const' }
    if pt.HasParentnode(nFormalParams) and (RoundBracketLevel(pt) > 0) then
    begin
      if pt.Word in [wVar, wConst] then
      begin
        Result := True;
        exit;
      end;
    end;

    { in procedure body - no return directly after 'if' }
    if InStatements(pt) then
    begin
      if pt.Word = wIf then
      begin
        Result := True;
        exit;
      end;
    end;
  end;

  { remove returns based on options }

  { the options don't apply after comments }
  if (pt.TokenType = ttComment) then
  begin
    Result := False;
    exit;
  end;

  { or just before them }
  ptNext := pt.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if ptNext = nil then
    exit;

  if (ptNext.TokenType = ttComment) or CommentBefore then
  begin
    Result := False;
    exit;
  end;

  if lcSetReturns.RemoveExpressionReturns and pt.HasParentNode(nExpression) then
  begin
    { can have a block that ends in expression without a semicolon, eg Don't remove return here:
      begin
        a := a + 2
      end;    }

    if ptNext.HasParentNode(nExpression) then
    begin
      Result := True;
      exit;
    end;
  end;

  if lcSetReturns.RemoveVarReturns and (pt.TokenType <> ttSemiColon) and
    (not (pt.Word in Declarations)) and pt.HasParentNode(nVarDecl) then
  begin
    if NoDeclarationBefore and NoSemicolonBefore and ptNext.HasParentNode(nVarDecl) then
    begin
      Result := True;
      exit;
    end;
  end;

  if lcSetReturns.RemoveProcedureDefReturns and pt.HasParentNode(nFormalParams) then
  begin
    Result := True;
    exit;
  end;

  if lcSetReturns.RemovePropertyReturns and (pt.TokenType <> ttSemiColon) and pt.HasParentNode(nProperty) then
  begin
    Result := True;
    exit;
  end;
end;

function TNoReturnAfter.NoDeclarationBefore: Boolean;
begin
  Result := (fcLastSolidToken = nil) or (not (fcLastSolidToken.Word in Declarations));
end;

function TNoReturnAfter.NoSemiColonBefore: Boolean;
begin
  Result := (fcLastSolidToken = nil) or (not (fcLastSolidToken.TokenType = ttSemiColon));
end;

function TNoReturnAfter.CommentBefore: Boolean;
begin
  Result := (fcLastSolidToken <> nil) and (fcLastSolidToken.TokenType = ttComment)
end;

procedure TNoReturnAfter.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType = ttReturn) and
    (fcLastSolidToken <> nil) and NeedsNoReturn(fcLastSolidToken) then
  begin
    // must repeat this until all done
    prVisitResult.Action := aDelete;
    fbDoneWork := True;
  end
  else
  begin

    { store for next time }
     if not (lcSourceToken.TokenType in [ttWhiteSpace, ttReturn]) then
      fcLastSolidToken := lcSourceToken;
  end;
end;

end.
