unit SpaceBeforeColon;

{ AFS 6 March 2003
  spaces (or not) before colon
}

interface

uses SwitchableVisitor, VisitParseTree;

type
  TSpaceBeforeColon = class(TSwitchableVisitor)
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;

  end;

implementation

uses
  JclStrings,
  JcfSettings, SetSpaces, SourceToken, TokenType, ParseTreeNodeType,
  FormatFlags, Nesting, TokenUtils;

function SpacesBefore(const pt: TSourceToken): integer;
var
  lcSpaces: TSetSpaces;
begin
  Assert(pt.TokenType = ttColon);

  lcSpaces := FormatSettings.Spaces;

  if pt.HasParentNode(nFormalParams) and InRoundBrackets(pt) then
  begin
    { in procedure params }
    Result := lcSpaces.SpacesBeforeColonParam;
  end
  else if pt.HasParentNode(nFunctionHeading) and not (pt.HasParentNode(nFormalParams)) then
  begin
    // function result type
    Result := lcSpaces.SpacesBeforeColonFn;
  end
  else if pt.HasParentNode(nVarSection) then
  begin
    // variable decl
    Result := lcSpaces.SpacesBeforeColonVar;
  end

  else if pt.HasParentNode(nConstSection) then
  begin
    // variable/const/type decl
    Result := lcSpaces.SpacesBeforeColonConst;
  end

  else if pt.HasParentNode(nTypeSection) then
  begin
    // type decl uses =, but there are colons in the fields of record defs and object types
    if pt.HasParentNode(ObjectTypes) then
      Result := lcSpaces.SpacesBeforeColonClassVar
    else if pt.HasParentNode(nRecordType) then
      Result := lcSpaces.SpacesBeforeColonRecordField
    else
    begin
      Result := 0;
      Assert(False, 'No context for colon ' + pt.DescribePosition);
    end;
  end
  else if pt.HasParentNode(nLabelDeclSection) then
  begin
    Result := lcSpaces.SpacesBeforeColonLabel;
  end
  else if InStatements(pt) then
  begin
    if IsCaseColon(pt) then
      Result := lcSpaces.SpacesBeforeColonCaseLabel
    else if IsLabelColon(pt) then
      Result := lcSpaces.SpacesBeforeColonLabel
    else
      Result := lcSpaces.SpacesBeforeColonLabel;
  end
  else if pt.HasParentNode(nAsm) then
  begin
    Result := 0; // !!!
  end
  else 
  begin
    Result := 0;
    // assertion failure brings the house down
    Assert(False, 'No context for colon ' + pt.DescribePosition);
  end;
end;


constructor TSpaceBeforeColon.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

procedure TSpaceBeforeColon.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcPrev: TSourceToken;
  liSpaces: integer;
begin
  lcSourceToken := TSourceToken(pcNode);
  if lcSourceToken = nil then
    exit;

  if lcSourceToken.TokenType <> ttColon then
    exit;

  lcPrev := lcSourceToken.PriorToken;
  if lcPrev = nil then
    exit;

  liSpaces := SpacesBefore(lcSourceToken);

  if liSpaces > 0 then
  begin
    { modify the existing previous space, or make a new one? }
    if (lcPrev.TokenType = ttWhiteSpace) then
    begin
      lcPrev.SourceCode := StrRepeat(AnsiSpace, liSpaces);
    end
    else
    begin
      prVisitResult.Action := aInsertBefore;
      prVisitResult.NewItem := NewSpace(liSpaces);
    end;
  end
  else
  begin
    { remove the space }
    if (lcPrev.TokenType = ttWhiteSpace) then
    begin
      lcPrev.SourceCode := '';
    end
    { else we are already right }
  end;

end;

end.
