unit Indenter;

{ AFS 23 Feb 2003
  process to indent tokens
  Will borrow some ond code, but mostly new
}

interface

uses SwitchableVisitor, VisitParseTree;


type
  TIndenter = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;

implementation

uses
  JclStrings,
  SourceToken, Nesting, FormatFlags, JcfSettings, TokenUtils,
  TokenType, ParseTreeNode, ParseTreeNodeType, WordMap;


function IsRunOnExpr(const pt: TSourceToken): boolean;
var
  lcExpr: TParseTreeNode;
  lcExprStart: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  lcExpr := pt.GetParentNode(nExpression);
  if lcExpr <> nil then
  begin
    lcExprStart := lcExpr.FirstLeaf as TSourceToken;
    while (lcExprStart <> nil) and (not lcExprStart.IsSolid) do
      lcExprStart := lcExprStart.NextToken;

    if lcExprStart.YPosition < pt.YPosition then
      Result := True;
  end;
end;

function IsInAssignExpr(const pt: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if not pt.HasParentNode(nAssignment) then
    exit;

  if not pt.IsOnRightOf(nAssignment, ttAssign) then
    exit;

  Result := True;
end;

function IsInProcedureParams(const pt: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if not pt.HasParentNode(nActualParams) then
    exit;

  if pt.Nestings.GetLevel(nlRoundBracket) = 0 then
    exit;

  // in a statement, in round brackets, also need .. ??

  Result := True;
end;

function CalculateIndent(const pt: TSourceToken): integer;
var
  liIndentCount: integer;
  lbHasIndentedRunOnLine: Boolean;
  lbHasIndentedDecl: boolean;
begin
  Result := 0;
  lbHasIndentedRunOnLine := False;
  lbHasIndentedDecl := False;

  if pt = nil then
    exit;

  { object types }
  if pt.HasParentNode(ObjectTypes) then
  begin
    if pt.Word in CLASS_VISIBILITY then
      liIndentCount := 1
    else if pt.Word = wEnd then
    begin
      // end is the end of the class unless it's the end of an anon record typed var
      if pt.HasParentNode(nRecordType) then
        liIndentCount := 2
      else
        liIndentCount := 1;
    end
    else
      liIndentCount := 2;

    // run on lines in procs
    if pt.HasParentNode(nProperty) and (pt.Word <> wProperty) and (pt.IndexOfSelf > 0) then
      inc(liIndentCount);

    if pt.HasParentNode(ProcedureHeadings) and (not (pt.Word in (ProcedureWords + [wClass]))) and (pt.IndexOfSelf > 0) then
      inc(liIndentCount);

    lbHasIndentedDecl := True;
  end

  { indent vars, consts etc, e.g.
    implementation
    const
      foo = 3;
    var
      bar: integer;
  }
  else if pt.HasParentNode(nDeclSection) and (not pt.HasParentNode(ProcedureNodes)) then
  begin
    if pt.Word in Declarations + ProcedureWords then
    begin
      {
        the words 'var' and 'cost' cna be found in proc params
        the words 'procedure' and 'function' can be found in type defs, e.g. type Tfoo = procedure of object; }
      if (pt.Word in ProcedureWords + ParamTypes) and
        (pt.HasParentNode(nProcedureType) or pt.HasParentNode(nFormalParams)) then
        liIndentCount := 1
      else
        liIndentCount := 0;
    end
    else
      liIndentCount := 1;

    if pt.Nestings.GetLevel(nlProcedure) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlProcedure) - 1);
  end
  else
  begin
    { this section is for
      - procedure body
      - procedure declarations
    }

    { indent procedure body for various kinds of block }
    liIndentCount := pt.Nestings.GetLevel(nlBlock);
    if liIndentCount > 0 then
    begin
      // outdent keywords that start and end the block
      if pt.Word in BlockOutdentWords then
      begin
        dec(liIndentCount);

        // not these in local record type decl
        if (pt.Word in [wCase, wEnd]) and (pt.HasParentNode(nRecordType)) then
          inc(liIndentCount);

        // not these in  procedure params or procedure type
        if (pt.Word in ParamTypes) and pt.HasParentNode(nProcedureType) then
            inc(liIndentCount);
      end
    end
    else
    begin
     { procedure formal params are not in the block }
      if pt.HasParentNode(nFormalParams) then
        inc(liIndentCount);
    end;

    if pt.Nestings.GetLevel(nlCaseSelector) > 0 then
    begin
      liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlCaseSelector);
      // don't indent the case label again
      if pt.HasParentNode(nCaseLabels, 5) then
        dec(liIndentCount)
      else if (pt.Word = wElse) and pt.HasParentNode(nElseCase, 1) then
        dec(liIndentCount);
    end;


    { nested procedures
    if pt.Nestings.GetLevel(nlProcedure) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlProcedure) - 1);
    }

    if pt.HasParentNode(nAsm) and pt.HasParentNode(nStatementList) then
      inc(liIndentCount);

    { indent for run on line }
    if pt.HasParentNode(nStatement) and (pt.Nestings.GetLevel(nlRoundBracket) + (pt.Nestings.GetLevel(nlSquareBracket)) > 0) then
    begin
      inc(liIndentCount);
      lbHasIndentedRunOnLine := True;
    end;

    if pt.HasParentNode(nUses) and (pt.Word <> wUses) then
      inc(liIndentCount);

    if (pt.Word = wOn) and pt.HasParentNode(nOnExceptionHandler, 1) then
      dec(liIndentCount);
  end;

  { record declaration stuph }
  if pt.HasParentNode(nRecordType) then
  begin
    { nested record types }
    if pt.Nestings.GetLevel(nlRecordType) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlRecordType) - 1);

    if pt.Nestings.GetLevel(nlRecordVariantSection) > 0 then
    begin
      liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlRecordVariantSection);
      if pt.Word = wCase then
        dec(liIndentCount);
    end;

    if pt.HasParentNode(nRecordVariant) and (RoundBracketLevel(pt) > 0) then
      inc(liIndentCount);

    if (pt.Word <> wEnd) then
    begin
      lbHasIndentedDecl := True;
      inc(liIndentCount);
    end;
  end;


  { these apply everywhere
    mostly because they need to apply to decls
    either in or out of a proc }

  if pt.HasParentNode(nEnumeratedType) and (RoundBracketLevel(pt) > 0) then
    inc(liIndentCount);

  { run on expression }
  if not lbHasIndentedRunOnLine then
  begin
    if IsRunOnExpr(pt) then
      inc(liIndentCount)
    else if IsInAssignExpr(pt) then
      inc(liIndentCount)
    else if IsInProcedureParams(pt) then
      inc(liIndentCount)
    { run-on type decl }
    else if pt.IsOnRightOf(nTypeDecl, wEquals) and (not lbHasIndentedDecl) and (pt.Word <> wEnd) then
      inc(liIndentCount);
  end;

  if pt.HasParentNode(nArrayConstant) and
    ((RoundBracketLevel(pt) > 0) or (pt.TokenType in [ttOpenBracket, ttCloseBracket])) then
    inc(liIndentCount);

  Assert(liIndentCount >= 0);

  Result := Settings.Indent.SpacesForIndentLevel(liIndentCount);
end;

constructor TIndenter.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eIndent];
end;

procedure TIndenter.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcPrev: TSourceToken;
  liPos: integer;
  liDesiredIndent: integer;
begin
  inherited;

  lcSourceToken := TSourceToken(pcNode);

  if IsFirstSolidTokenOnLine(lcSourceToken) then
  begin
    liDesiredIndent := CalculateIndent(lcSourceToken);
    liPos := lcSourceToken.XPosition - 1;
    Assert(liPos >= 0);

    if liDesiredIndent < liPos then
    begin
      { delete some spaces before, if they exist }
      lcPrev := lcSourceToken.PriorToken;
      if (lcPrev <> nil) and (lcPrev.TokenType = ttWhiteSpace) then
      begin
        lcPrev.SourceCode := StrRepeat(AnsiSpace, liDesiredIndent - lcPrev.XPosition + 1);
      end
      else if liDesiredIndent > 0 then
      begin
        // no prev ? Insert one
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewSpace(liDesiredIndent);
      end;

    end
    else if liDesiredIndent > liPos then
    begin
      prVisitResult.Action := aInsertBefore;
      prVisitResult.NewItem := NewSpace(liDesiredIndent - liPos);
    end;
  end;

end;

end.
