unit NoReturnBefore;

{ AFS 11 Jan 2003
  Some tokens should not have a return before them for fomatting
}

interface

uses SwitchableVisitor, VisitParseTree;


type
  TNoReturnBefore = class(TSwitchableVisitor)
  private
    fbSafeToRemoveReturn: boolean;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

  public
    constructor Create; override;
  end;

implementation

uses SourceToken, WordMap, TokenUtils, TokenType, ParseTreeNodeType,
  JcfSettings, SetReturns, FormatFlags;

function HasNoReturnBefore(const pt: TSourceToken): boolean;
const
  NoReturnTokens: TTokenTypeSet = [ttAssign, ttOperator, ttColon, ttSemiColon];
  ProcNoReturnWords: TWordSet   = [wThen, wDo];
begin
  Result := False;

  if pt = nil then
    exit;

  if (pt.TokenType in NoReturnTokens) then
  begin
    Result := True;
    exit;
  end;

  { no return before then and do  in procedure body }
  if (pt.Word in ProcNoReturnWords) and InStatements(pt) then
  begin
    Result := True;
    exit;
  end;

  { no return in record def before the record keyword, likewise class & interface
    be carefull with the word 'class' as it also denotes (static) class fns. }
  if pt.HasParentNode(nTypeDecl) and (pt.Word in StructuredTypeWords) and
    (not pt.HasParentNode(nClassVisibility)) then
  begin
    Result := True;
    exit;
  end;
end;

constructor TNoReturnBefore.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

procedure TNoReturnBefore.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  // not safe to remove return at a comment like this
  if (lcSourceToken.TokenType = ttComment) and (lcSourceToken.CommentStyle = eDoubleSlash) then
    fbSafeToRemoveReturn := False
  else if (lcSourceToken.TokenType <> ttReturn) then
    fbSafeToRemoveReturn := True;
  // safe again after the next return

  if (lcSourceToken.TokenType = ttReturn) and fbSafeToRemoveReturn then
  begin
    lcNext := lcSourceToken.NextSolidToken;

    if HasNoReturnBefore(lcNext) then
    begin
      prVisitResult.Action := aDelete;
    end;
  end;

end;

end.
