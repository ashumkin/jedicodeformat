unit RemoveSpaceAtLineEnd;

{ AFS 10 May 2003
  remove trainling spaces on lines
  makes test fail, false delta }

interface

uses SwitchableVisitor, VisitParseTree;


type
  TRemoveSpaceAtLineEnd = class(TSwitchableVisitor)
    private
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;



implementation

uses  FormatFlags, SourceToken, TokenType;


constructor TRemoveSpaceAtLineEnd.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

procedure TRemoveSpaceAtLineEnd.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { is this white space? }
  if lcSourceToken.TokenType = ttWhiteSpace then
  begin
    { is a return next ? }
    lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
    if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    begin
      lcSourceToken.SourceCode := '';
    end;
  end;

end;

end.
