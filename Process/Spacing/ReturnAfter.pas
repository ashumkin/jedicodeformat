unit ReturnAfter;

{ AFS 7 Jan 2003
  Some tokens need a return after them for fomatting
}

interface

uses BaseVisitor, VisitParseTree;


type
  TReturnAfter = class(TBaseTreeNodeVisitor)
    private

    public

      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  JclStrings,
  JcfMiscFunctions,
  TokenUtils, SourceToken, TokenType, WordMap, Nesting,
  ParseTreeNodeType, JcfSettings;

const
  WordsJustReturnAfter: TWordSet = [wType, wBegin, wRepeat,
    wTry, wExcept, wFinally, wLabel,
    wInitialization, wFinalization];
  // can't add 'interface' as it has a second meaning :(

  { blank line is 2 returns }
  WordsBlankLineAfter: TWordSet = [wImplementation];

{ semicolons have returns after them except for a few places
   1) before and between procedure directives, e.g. procedure Fred; virtual; safecall;
   2) The array property directive 'default' has a semicolon before it
   3) seperating fields of a const record declaration
   4) between params in a procedure declaration or header
   5) as 4, in a procedure type in a type def
}
function SemicolonHasReturn(const pt: TSourceToken): boolean;
begin
  Result := True;

  { point 1 }
  if (pt.HasParentNode(nProcedureDirectives)) then
  begin
    Result := False;
    exit;
  end;

  { point 2}
  if pt.HasParentNode(nPropertySpecifier) then
  begin
    Result := False;
    exit;
  end;

  { point 3 }
  if pt.HasParentNode(nRecordConstant) then
  begin
    Result := False;
    exit;
  end;

  { point 4 }
  if (pt.HasParentNode(nFormalParams)) then
  begin
    Result := False;
    exit;
  end;

  { point 4, for a procedure type def }
  if pt.HasParentNode(nProcedureType) then
  begin
    Result := False;
    exit;
  end;

  { in a record type def }
  if pt.HasParentNode(nRecordType) then
  begin
    Result := True;
    exit;
  end;
end;

function NeedsBlankLine(const pt, ptNext: TSourceToken): boolean;
begin
  Result := False;

  if (pt.TokenType in ReservedWordTokens) and (pt.Word in WordsBlankLineAfter) then
  begin
    Result := True;
    exit;
  end;

  { 'interface', but not as a typedef, but as the section }
  if (pt.Word = wInterface) and pt.HasParentNode(nInterfaceSection, 1) then
  begin
    Result := True;
    exit;
  end;

  { semicolon that ends a proc or is between procs e.g. end of uses clause }
  if (pt.TokenType = ttSemiColon) and
    (not pt.HasParentNode(ProcedureNodes)) and
    (BlockLevel(pt) = 0) and
    (not pt.HasParentNode(nDeclSection)) then

  begin
    Result := True;
    exit;
  end;
end;


function NeedsReturn(const pt, ptNext: TSourceToken): boolean;
begin
  Result := False;

  if (pt.TokenType in ReservedWordTokens) and (pt.Word in WordsJustReturnAfter) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType = ttSemiColon) then
  begin
    Result := SemicolonHasReturn(pt);
    if Result then
      exit;
  end;

  { var and const when not in procedure parameters or array properties }
  if (pt.Word in [wVar, wThreadVar, wConst, wResourceString]) and
    pt.HasParentNode([nVarSection, nConstSection]) then
  begin
    Result := True;
    exit;
  end;

  { return after else unless there is an in }
  if (pt.Word = wElse) and (ptNext.Word <> wIf) then
  begin
    Result := True;
    exit;
  end;

  { case .. of  }
  if (pt.Word = wOf) and (pt.IsOnRightOf(nCaseStatement, wCase)) then
  begin
    Result := True;
    exit;
  end;

  { end without semicolon or dot, or hint directive }
  if (pt.Word = wEnd) and (not (ptNext.TokenType in [ttSemiColon, ttDot]))  and
    (not (ptNext.Word in HintDirectives)) then
  begin
    Result := True;
    exit;
  end;

  { access specifiying directive (private, public et al) in a class def }
  if pt.HasParentNode(nClassVisibility) and (pt.Word in CLASS_VISIBILITY) then
  begin
    Result := True;
    exit;
  end;

  { comma in exports clause }
  if (pt.TokenType = ttComma) and pt.HasParentNode(nExports) then
  begin
    Result := True;
    exit;
  end;

  { comma in uses clause of program or lib - these are 1 per line,
    using the 'in' keyword to specify the file  }
  if (pt.TokenType = ttComma) and pt.HasParentNode(nUses) and pt.IsOnRightOf(nUsesItem, wOf) then
  begin
    Result := True;
    exit;
  end;

  if (pt.Word = wRecord) and pt.IsOnRightOf(nFieldDeclaration, ttColon) then
  begin
    Result := True;
    exit;
  end;

  { end of class heritage }
  if (pt.HasParentNode(nRestrictedType)) and
    (not pt.HasParentNode(nClassVisibility)) and
    (ptNext.HasParentNode(nClassVisibility)) then
  begin
    Result := True;
    exit;
  end;

  { return in record def after the record keyword }
  if pt.HasParentNode(nRecordType) and (pt.Word = wRecord) then
  begin
    Result := True;
    exit;
  end;


  if NeedsBlankLine(pt, ptNext) then
  begin
    Result := True;
    exit;
  end;
end;

procedure TReturnAfter.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcNext, lcCommentTest: TSourceToken;
  liReturnsNeeded: integer;
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { check the next significant token  }
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttComment]);

  if not NeedsReturn(lcSourceToken, lcNext) then
    exit;

  { catch comments!

    if the token needs a return after but the next thing is a // comment, then leave as is
    ie don't turn
      if (a > 20) then // catch large values
      begin
        ...
    into
      if (a > 20) then
      // catch large values
      begin
        ... }
  lcCommentTest := lcNext.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if (lcCommentTest.TokenType = ttComment) then
    exit;

  liReturnsNeeded := 0;
  if (lcNext.TokenType <> ttReturn) then
  begin
    { no returns at all }
    inc(liReturnsNeeded);
    if NeedsBlankLine(lcSourceToken, lcNext) then
      inc(liReturnsNeeded);
  end
  else
  begin
    { one return }
    if NeedsBlankLine(lcSourceToken, lcNext) then
    begin
      { check for a second return }
      lcNext := lcNext.NextTokenWithExclusions([ttWhiteSpace]);
      if (lcNext.TokenType <> ttReturn) then
        inc(liReturnsNeeded);
    end;
  end;

  case liReturnsNeeded of
    0:  ;
    1:
    begin
      prVisitResult.Action := aInsertAfter;
      prVisitResult.NewItem := NewReturn;
    end;
    2:
    begin
      prVisitResult.Action := aInsertAfter;
      prVisitResult.NewItem := NewReturn;
      prVisitResult.NewItem2 := NewReturn;
    end;
    else
    begin
      Assert(False, 'Too many returns');
    end;
  end;

end;

end.
