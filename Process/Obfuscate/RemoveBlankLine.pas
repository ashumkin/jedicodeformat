unit RemoveBlankLine;

{ AFS 17 Jan 2003
  Obfuscate - remove blank lines }

interface

uses SwitchableVisitor, VisitParseTree;

type
  TRemoveBlankLine = class(TSwitchableVisitor)
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;

implementation

uses ParseTreeNode, SourceToken, TokenType, ParseTreeNodeType, FormatFlags;


constructor TRemoveBlankLine.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TRemoveBlankLine.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcNext: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  { find next, excluding spaces and comments, except '//' comment }
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
  while (lcNext <> nil) and (lcNext.TokenType = ttComment) and (lcNext.CommentStyle <> eDoubleSlash) do
    lcNext := lcNext.NextTokenWithExclusions([ttWhiteSpace]);

  {
    A return, followed by another return (with nothing of substance between them)
    is a blank line, so kill one of them
    thia pplies even in ASM blocks  }
  if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    prVisitResult.Action := aDelete;

end;

end.
