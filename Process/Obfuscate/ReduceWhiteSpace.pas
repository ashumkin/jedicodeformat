unit ReduceWhiteSpace;

{ AFS 28 Dec 2002

  Visitor to reduce all whitespace to single spaces
  Obfuscation
}

interface

uses SwitchableVisitor, VisitParseTree;

type
  TReduceWhiteSpace = class(TSwitchableVisitor)
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;


implementation

uses SourceToken, TokenType, FormatFlags;

constructor TReduceWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TReduceWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttWhiteSpace then
    lcSourceToken.SourceCode := ' ';
end;

end.
