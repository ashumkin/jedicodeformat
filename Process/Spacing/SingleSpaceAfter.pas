unit SingleSpaceAfter;

interface

{ AFS 9 Dec 1999
  Single space after : }

uses SwitchableVisitor, VisitParseTree;


type
  TSingleSpaceAfter = class(TSwitchableVisitor)
    private
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;


implementation

uses
  JclStrings,
  JcfMiscFunctions,
  SourceToken, TokenType, WordMap, ParseTreeNodeType, JcfSettings,
  FormatFlags, TokenUtils;

  const
  SingleSpaceAfterTokens: TTokenTypeSet = [ttColon, ttAssign, ttComma];
  SingleSpaceAfterWords: TWordSet       = [wProcedure, wFunction,
    wConstructor, wDestructor, wProperty,
    wOf, wDo, wWhile, wUntil, wCase, wIf, wTo, wDownTo,

    // some unary operators
    wNot,
    // all operators that are always binary
    wAnd, wAs, wDiv, wIn, wIs, wMod, wOr, wShl, wShr, wXor,
    wTimes, wFloatDiv, wEquals, wGreaterThan, wLessThan,
    wGreaterThanOrEqual, wLessThanOrEqual, wNotEqual];

  PossiblyUnaryOperators: TWordSet = [wPlus, wMinus];

function NeedsSingleSpace(const pt, ptNext: TSourceToken): boolean;
begin
  Result := False;

  if pt.HasParentNode(nAsm) then
    exit;

  // if the next token is a comment, leave it where it is, do not adjust spacing
  if ptNext.TokenType = ttComment then
    exit;

  { semciolon as a record field seperator in a const record declaration
   has no newline (See ReturnAfter.pas), just a single space }
  if (pt.TokenType = ttSemiColon) and (pt.HasParentNode(nRecordConstant)) then
  begin
    Result := True;
    exit;
  end;

  { semicolon  in param  declaration list }
  if (pt.TokenType = ttSemiColon) and (pt.HasParentNode(nFormalParams)) then
  begin
    Result := True;
    exit;
  end;

  { semicolon in param lists in proc type def. as above }
  if (pt.TokenType = ttSemiColon) and (pt.HasParentNode(nProcedureType)) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType in SingleSpaceAfterTokens) then
  begin
    Result := True;
    exit;
  end;

  if (pt.Word in SingleSpaceAfterWords) then
  begin
    Result := True;
    exit;
  end;

  { + or - but only if it is a binary operator, ie a term to the left of it }
  if (pt.Word in PossiblyUnaryOperators) and (pt.HasParentNode(nExpression)) and
    (not IsUnaryOperator(pt)) then
  begin
    Result := True;
    exit;
  end;

  { only if it actually is a directive, see TestCases/TestBogusDirectives for details }
  if (pt.Word in AllDirectives) and (pt.HasParentNode(DirectiveNodes)) then
  begin
    Result := True;
    exit;
  end;

  if pt.Word = wEquals then
  begin
    Result := True;
    exit;
  end;

  { 'in' in the uses clause }
  if (pt.Word = wIn) and (pt.HasParentNode(nUses)) then
  begin
    Result := True;
    exit;
  end;

  { const or var as parameter var types }
  if (pt.Word in ParamTypes) and (pt.HasParentNode(nFormalParams)) then
  begin
    Result := True;
    exit;
  end;

  if (pt.Word in ParamTypes) and pt.HasParentNode(nPropertyParameterList) and
     pt.IsOnRightOf(nPropertyParameterList, ttOpenSquareBracket) then
  begin
    Result := True;
    exit;
  end;

  { single space before class heritage ?
    see NoSpaceAfter }
  if (pt.HasParentNode(nRestrictedType)) and (pt.Word in ObjectTypeWords) and
    (Settings.Spaces.SpaceBeforeClassHeritage) then
  begin
    if (ptNext.TokenType in [ttOpenBracket, ttSemiColon]) then
    begin
      Result := True;
      exit;
    end;
  end;
end;


constructor TSingleSpaceAfter.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace, eRemoveReturn];
end;

procedure TSingleSpaceAfter.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext, lcNew: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { exclude if a comment is next }
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if lcNext = nil then
    exit;

  if lcNext.TokenType = ttComment then
    exit;

  if NeedsSingleSpace(lcSourceToken, lcNext) then
  begin
    { inspect the next token }
    lcNext := lcSourceToken.NextToken;
    if lcNext.TokenType = ttWhiteSpace then
    begin
      lcNext.SourceCode := AnsiSpace;
    end
    else if (lcNext.TokenType <> ttReturn) then
    begin
      // insert a space
      lcNew := TSourceToken.Create;
      lcNew.TokenType := ttWhiteSpace;
      lcNew.SourceCode := AnsiSpace;

      prVisitResult.Action := aInsertAfter;
      prVisitResult.NewItem := lcNew;
    end;

  end;
end;

end.
