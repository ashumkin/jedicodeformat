unit NoSpaceAfter;

{ AFS 9 Dec 1999
  no space after  certain tokens }

interface

uses BaseVisitor, VisitParseTree;


type
  TNoSpaceAfter = class(TBaseTreeNodeVisitor)
    private
      fbSafeToRemoveReturn: boolean;  // this taken from NoReturnBefore

    public
      constructor Create; override;

      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  JcfMiscFunctions,
  SourceToken, TokenType, WordMap, ParseTreeNodeType, JcfSettings;

{ TNoSpaceAfter }


function NeedsNoSpace(const pt, ptNext: TSourceToken): boolean;
const
  NoSpaceAnywhere: TTokenTypeSet = [ttOpenBracket, ttOpenSquareBracket, ttDot];
begin
  Result := False;

  { if the next thing is a comment, leave well enough alone }
  if ptNext.TokenType = ttComment then
    exit;

  if pt.TokenType in NoSpaceAnywhere then
  begin
    Result := True;
    exit;
  end;

  { no space between method name and open bracket for param list
    no space between type & bracket for cast
    no space between fn name & params for procedure call }
  if (pt.HasParentNode([nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl, nStatementList]) and
    (pt.TokenType in [ttWord, ttBuiltInType])) then
  begin
    if (ptNext.TokenType in OpenBrackets) then
    begin
      Result := True;
      exit;
    end;
  end;

  { the above takes care of procedure headers but not procedure type defs
   eg type TFred = procedure(i: integer) of object;
    note no space before the open bracket }
   if pt.HasParentNode(nTypeDecl) and (pt.IsOnRightOf(nTypeDecl, wEquals)) and (pt.Word in ProcedureWords) then
  begin
    if (ptNext.TokenType in OpenBrackets) then
    begin
      Result := True;
      exit;
    end;
  end;

  { no space after unary operator in expression }
  if (pt.HasParentNode(nExpression)) and
    (pt.TokenType = ttOperator) and (pt.Word in PossiblyUnarySymbolOperators) and
    (not StrHasAlpha(pt.SourceCode)) then
  begin
    Result := True;
    exit;
  end;

  { no space before class heritage ? could be one of 3 things
    TFoo = class; - no space, but "No space before semicolon" should take care of that
    TBar = class(TBaz) - no space unless you are Marcel van Brakel
    TWibble = class of TFish - has space

    see SingleSpaceAfter

    also applies to type TFoo = interface(IDispatch) }
  if (pt.HasParentNode(nRestrictedType)) and (pt.Word in ObjectTypeWords)
    and (not (Settings.Spaces.SpaceBeforeClassHeritage)) then
  begin
    if (ptNext.TokenType in [ttOpenBracket, ttSemiColon]) then
    begin
      Result := True;
      exit;
    end;
  end;
end;


constructor TNoSpaceAfter.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
end;

procedure TNoSpaceAfter.VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);

  if NeedsNoSpace(lcSourceToken, lcNext) then
    prVisitResult.Action := aDeleteNext;

end;

end.
