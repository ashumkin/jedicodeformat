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
  TokenType, ParseTreeNodeType, WordMap;

function CalculateIndent(const pt: TSourceToken): integer;
var
  liIndentCount: integer;
begin
  Result := 0;

  if pt = nil then
    exit;

  { indent for various kinds of block }
  liIndentCount := pt.Nestings.GetLevel(nlBlock);
  if liIndentCount > 0 then
  begin
    // outdent keywords that start and end the block
    if pt.Word in BlockOutdentWords then
      dec(liIndentCount);
  end;

  liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlRecordVariantSection);
  liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlCaseStatement);
  liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlTryBlock);
  liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlFinallyBlock);
  liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlExceptBlock);

  { nested procedures }
  if pt.Nestings.GetLevel(nlProcedure) > 1 then
    liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlProcedure) - 1);

  { indent for run on line }
  if (pt.Nestings.GetLevel(nlRoundBracket) + (pt.Nestings.GetLevel(nlSquareBracket)) > 0) then
    liIndentCount := liIndentCount + 1;

  { indent in clas defs }
  if pt.HasParentNode(nClassBody) then
  begin
    if pt.Word in CLASS_VISIBILITY then
      liIndentCount := 1
    else
      liIndentCount := 2;
  end;

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
