unit ReturnBefore;

{ AFS 10 Jan 2003
  Return before
}

interface

uses SwitchableVisitor, VisitParseTree;


type
  TReturnBefore = class(TSwitchableVisitor)
    private
      fiReturnsBefore, fiNextReturnsBefore: integer;
    protected
      procedure InspectSourceToken(const pcToken: TObject); override;

      procedure EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;


implementation

uses
  JclStrings,
  JcfMiscFunctions, TokenUtils,
  SourceToken, TokenType, WordMap, Nesting, ParseTreeNodeType, JcfSettings,
  FormatFlags;

const
  WordsReturnBefore: TWordSet =
    [wBegin, wEnd, wUntil, wElse, wTry, wFinally, wExcept];

  WordsBlankLineBefore: TWordSet =
    [wImplementation, wInitialization, wFinalization, wUses];


function NeedsBlankLine(const pt, ptNext: TSourceToken): boolean;
begin
  Result := (pt.Word in WordsBlankLineBefore);
  if Result then
    exit;

  { function/proc body needs a blank line
   but not in RHSEquals of type defs,
   but not in class & interface def,
   but not if precedeed by the class specified for class functions
   but not if it is a contained function

   !! this mistakenly spaces proc forwards.
   }

  if (pt.Word in [wProcedure, wFunction]) and  // pt.ProcedureHasBody and
    (not pt.HasParentNode(nDeclSection)) and
    (not IsClassFunction(pt)) and (pt.Nestings.GetLevel(nlProcedure) = 0) then
  begin
    Result := True;
    exit;
  end;

    { blank line before the words var, type or const at top level
      except for:
      type t2 = type integer; }
  if (pt.Word in Declarations) and (pt.Nestings.Total = 0) and
    (not pt.IsOnRightOf(nTypeDecl, wEquals)) then
  begin
    Result := True;
    exit;
  end;

  { start of class function body }
  if (pt.Word = wClass) and (IsClassFunction(pt)) and
    (not pt.HasParentNode(nDeclSection)) and
    (pt.HasParentNode(nImplementationSection)) then
  begin
    Result := True;
    exit;
  end;

  { interface, but not as a typedef }
  if (pt.Word = wInterface) and not (pt.HasParentNode(nTypeDecl)) then
  begin
    Result := True;
    exit;
  end;

  { end. don't always want this, not in init block  }
  if (pt.Word = wEnd) and (ptNext.TokenType = ttDot) and pt.HasParentNode(nUnit, 2)
    and (BlockLevel(pt) < 1) then
  begin
    Result := True;
    exit;
  end;
end;


function NeedsReturn(const pt, ptNext: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  Result := (pt.Word in WordsReturnBefore);
  if Result = True then
    exit;

  { there is not always a return before 'type'
    e.g.
    type TMyInteger = type Integer;
    is legal, only a return before the first one

   var, const, type but not in parameter list }
  if (pt.Word in Declarations) and pt.HasParentNode(nTopLevelSections, 1)
    and (not pt.IsOnRightOf(nTypeDecl, wEquals)) then
  begin
    Result := True;
    exit;
  end;

  { procedure & function in class def get return but not blank line before }
  if (pt.Word in ProcedureWords + [wProperty]) and
    (pt.HasParentNode([nClassType, nClassType])) and
    (not IsClassFunction(pt)) then
  begin
    Result := True;
    exit;
  end;

  { nested procs get it as well }
  if (pt.Word in ProcedureWords) and (not pt.HasParentNode(nProcedureDecl)) and
    (not IsClassFunction(pt)) and
    (not pt.HasParentNode(nType)) then
  begin
    Result := True;
    exit;
  end;

  { class function }
  if (pt.Word = wClass) and pt.HasParentNode(nProcedureDecl) then
  begin
    Result := True;
    exit;
  end;

  { access specifiying directive (private, public et al) in a class def }
  if pt.HasParentNode(nClassType) and IsClassDirective(pt) then
  begin
    Result := True;
    exit;
  end;

  { "uses UnitName in 'File'" has a blank line before UnitName }
  if (pt.TokenType = ttWord) and (pt.HasParentNode(nUses)) and (ptNext.Word = wIn) then
  begin
    Result := True;
    exit;
  end;
end;

constructor TReturnBefore.Create;
begin
  inherited;
  fiReturnsBefore := 0;
  fiNextReturnsBefore := 0;
  FormatFlags := FormatFlags + [eAddReturn];
end;

procedure TReturnBefore.EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  liReturnsNeeded: integer;
begin
  lcSourceToken := TSourceToken(pcToken);
  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  liReturnsNeeded := 0;

  if NeedsBlankLine(lcSourceToken, lcNext) then
    liReturnsNeeded := 2
  else if NeedsReturn(lcSourceToken, lcNext) then
    liReturnsNeeded := 1;


  { number to insert = needed - actual }
  liReturnsNeeded := liReturnsNeeded - fiReturnsBefore;

  if liReturnsNeeded > 0 then
  begin

    case liReturnsNeeded of
      1:
      begin
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewReturn;
      end;
      2:
      begin
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewReturn;
        prVisitResult.NewItem2 := NewReturn;
      end;
      else
      begin
        Assert(False, 'Too many returns');
      end;
    end;
  end;

end;

procedure TReturnBefore.InspectSourceToken(const pcToken: TObject);
var
  lcSourceToken: TSourceToken;
begin
  {
    inspect the tokens as they go past
    this is a running total, that is affeced by returns & non-white-space chars
   A comment line is as good as a blank line for this

    if we encounter the tokens <return> <spaces> <word-needing-return before> the flag must be set true
   }
   fiReturnsBefore := fiNextReturnsBefore;

  lcSourceToken := TSourceToken(pcToken);

  if (lcSourceToken.TokenType = ttReturn) then
    inc(fiNextReturnsBefore)
  else if not (lcSourceToken.TokenType in [ttReturn, ttWhiteSpace, ttComment]) then
    fiNextReturnsBefore := 0;

end;

end.
