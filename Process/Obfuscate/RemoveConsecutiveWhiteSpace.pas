unit RemoveConsecutiveWhiteSpace;

{
 AFS 29 Dec 2002

  Visitor to remove consecutive whitespace
  Obfuscation
}

interface

uses SwitchableVisitor, VisitParseTree;

type
  TRemoveConsecutiveWhiteSpace = class(TSwitchableVisitor)
  private
    fbWhiteSpaceLast: boolean;
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;


implementation

uses SourceToken, TokenType, FormatFlags;

constructor TRemoveConsecutiveWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TRemoveConsecutiveWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { delete whitespace if the last one was also whitespace }
  if (lcSourceToken.TokenType = ttWhiteSpace) and fbWhiteSpaceLast then
    prVisitResult.action := aDelete;

  fbWhiteSpaceLast := (lcSourceToken.TokenType = ttWhiteSpace);
end;

end.

