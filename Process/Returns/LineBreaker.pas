unit LineBreaker;

{ AFS 10 March 2003
  With the Indenter, this is the other big & inportant process.
  Its job is to break long lines.
  This is more complex than most.
  If the line is too long, then the entire line is inspected
  and each token thereon is given a score
  The token with the 'best' (lowest? Highest?) score is where
  (before? after?) the line will be broken 
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


{ TIndenter }

constructor TIndenter.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eLineBreaking];
end;

procedure TIndenter.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcLastOnLine: TSourceToken;
  lcNext: TSourceToken;
  liIndexOfFirstSolidToken: integer;
  liCount: integer;
  liWidth: integer;
begin
  inherited;

  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  // read until the next return
  lcNext := lcSourceToken.NextToken;
  liIndexOfFirstSolidToken := -1;
  liCount := 0;
  liWidth := 0;

  while (lcNext <> nil) and (lcNext.TokenType <> ttReturn) do
  begin
    { record which token starts the line's solid text - don't want to break before it }
    if (lcNext.TokenType <> ttWhiteSpace) and (liIndexOfFirstSolidToken = -1) then
      liIndexOfFirstSolidToken := liCount;

    Inc(liCount);
    liWidth := liWidth + Length(lcNext.SourceCode);

    lcLastOnLine := lcNext;
    lcNext := lcNext.NextToken;
  end;

  // EOF or blank line means no linebreaking to do 
  if (lcNext = nil) or (lcNext = lcSourceToken) then
    exit;

  { must be solid stuff on the line }
  if liIndexOfFirstSolidToken < 0 then
    exit;

  { if the line does not run on, exit now }
  if liWidth < Settings.Returns.MaxLineLength then
    exit;

  { right, the line is too long.
    Score each token to find the best place to break
    This is a bunch of heuristics to produce a reasonably aesthetic result
    The obvious heuristics are:
     - it is better to break near the specified max line length (and bad to break near the line start)
     - it is better to break outside of brackets
     - it is good to break after operators like '+' 'or' ',' (and bad to break before them)
  }

end;

end.
