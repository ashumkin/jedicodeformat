unit LongLineBreaker;

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

uses SwitchableVisitor, VisitParseTree, IntList, SourceTokenList;


type
  TLongLineBreaker = class(TSwitchableVisitor)
  private
    lcScores: TIntList;
    lcTokens: TSourceTokenList;

    fiIndexOfFirstSolidToken: integer;

    function PositionScore(const piIndex, piPos: integer): integer;
    procedure FixPos(piStart, piEnd: integer);
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  SourceToken, Nesting, FormatFlags, JcfSettings, TokenUtils,
  TokenType, ParseTreeNode, ParseTreeNodeType, WordMap;

{ TLongLineBreaker }

constructor TLongLineBreaker.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eLineBreaking];
  lcScores := TIntList.Create;
  lcTokens := TSourceTokenList.Create;
end;

destructor TLongLineBreaker.Destroy;
begin
  FreeAndNil(lcScores);
  FreeAndNil(lcTokens);
  inherited;
end;

procedure TLongLineBreaker.FixPos(piStart, piEnd: integer);
var
  liLoop, liPos: integer;
  lt: TSourceToken;
begin
  liPos := 0;
  for liLoop := piStart to piEnd do
  begin
    lt := lcTokens.SourceTokens[liLoop];
    lt.XPosition := liPos;

    case lt.TokenType of
      ttEOF: break;
      ttReturn: liPos := 0;
      else
        liPos := liPos + Length(lt.SourceCode);
    end;
  end;
end;


function TLongLineBreaker.PositionScore(const piIndex, piPos: integer): integer;
const
  WIDTH_SCORE_FACTOR = 5;
  TO_FAR_SCORE_FACTOR = 5;
  FAR_TO_FAR_SCORE_FACTOR = 0.3;
  NOGO_PLACE = -100;
  PLATEAU    = 50;
  ROOFSLOPE  = 7;
var
  liEffectiveWidth: integer;
  liMidPoint, liThreeQuarterPoint: integer;
  liClose:    integer;
  liOverFlow: integer;
  fUnderflow: double;
begin
  if piIndex < fiIndexOfFirstSolidToken then
  begin
    Result := NOGO_PLACE;
    exit;
  end;

  { middle of the actual line (ie from first token pos to max length) }
  liEffectiveWidth := Settings.Returns.MaxLineLength - fiIndexOfFirstSolidToken;
  liMidPoint       := (liEffectiveWidth div 2) + fiIndexOfFirstSolidToken;

  if piPos < liMidPoint then
  begin
    { slope up evenly over the first half }
    Result := NOGO_PLACE + ((PLATEAU - NOGO_PLACE) * piPos * 2) div liEffectiveWidth;
  end
  else if piPos <= Settings.Returns.MaxLineLength then
  begin
    { relatively flat plateau, slight bump in the middle }
    liThreeQuarterPoint := (liEffectiveWidth * 3 div 4) + fiIndexOfFirstSolidToken;
    { how close to it }
    liClose := (liEffectiveWidth div 4) - abs(piPos - liThreeQuarterPoint) + 1;
    Assert(liClose >= 0);

    Result := PLATEAU + (liClose div ROOFSLOPE);
  end
  else
  begin
    { past the end}
    liOverFlow := piPos - Settings.Returns.MaxLineLength;
    Result     := PLATEAU - (liOverFlow * TO_FAR_SCORE_FACTOR);
    if Result < NOGO_PLACE then
    begin
      fUnderflow := NOGO_PLACE - Result;
      { must make is slightly lower the further we go -
      otherwise the last pos is found in lines that are far too long with no good place to break
      eg lines that start with a very long text string }
      Result     := NOGO_PLACE - Round(fUnderflow * FAR_TO_FAR_SCORE_FACTOR);
    end;
  end;
end;


{ experimental - score for line breaking based on the parse tree
  The idea is that higher up the tree is going to be a better place to break
  as it represents a natural break in the program flow

  larger number are better so invert
}
function TreeScore(const pcToken: TSourceToken): integer;
const
  DEPTH_FACTOR = 5;
  FIRST_CHILD_FACTOR = 20;
begin
  Result := - (pcToken.Level * DEPTH_FACTOR);
  if pcToken.IndexOfSelf = 0 then
    Result := Result + FIRST_CHILD_FACTOR;
end;

{ scoring - based on the current token,
  score how aestetically pleasing a line break after this token would be }
procedure ScoreToken(const pcToken: TSourceToken;
  var piScoreBefore, piScoreAfter: integer);
const
  VERY_BAD_PLACE  = -20;
  BAD_PLACE       = -10;
  SEMI_BAD_PLACE  = -5;
  SEMI_GOOD_PLACE = 5;
  GOOD_PLACE      = 10;
  EXCELLENT_PLACE = 20;
  AWESOME_PLACE   = 30;
  BRACKET_SCALE   = -8;
begin
  Assert(pcToken <> nil);

  case pcToken.TokenType of

    { bad to break just before or after a dot.
      However if you must pick, break after it  }
    ttDot:
    begin
      piScoreBefore := VERY_BAD_PLACE;
      piScoreAfter := BAD_PLACE;
    end;
    { it is Goodish to break after a colon
      unless in a parameter list or var declaration }
    ttColon:
    begin
      if InDeclarations(pcToken) or pcToken.HasParentNode(nFormalParams) then
      begin
        piScoreBefore := BAD_PLACE;
        piScoreAfter := VERY_BAD_PLACE;
      end
      else
      begin
        piScoreBefore := SEMI_BAD_PLACE;
        piScoreAfter := SEMI_GOOD_PLACE;
      end;
    end;
    { or just before close brackets or a semicolon -
      better to break after these }
    ttCloseBracket, ttCloseSquareBracket:
    begin
     piScoreBefore := VERY_BAD_PLACE;
      piScoreAfter := GOOD_PLACE;
    end;
    { break after the semicolon is awesome, before is terrible}
    ttSemiColon:
    begin
      piScoreBefore := VERY_BAD_PLACE;
      piScoreAfter := AWESOME_PLACE;
    end;
    { It is good to break after := or comma, not before }
    ttAssign, ttComma:
    begin
      piScoreBefore := VERY_BAD_PLACE;
      piScoreAfter := EXCELLENT_PLACE;
    end;
    ttOperator:
    begin
      { good to break after an operator (except unary operators)
      bad to break just before one }
      if not IsUnaryOperator(pcToken) then
      begin
        piScoreAfter := GOOD_PLACE;
        piScoreBefore := VERY_BAD_PLACE;
      end
      else
        { dont break between unary operator and operand }
        piScoreAfter := VERY_BAD_PLACE;
    end;

    { break before white Space, not after }
    ttWhiteSpace:
    begin
      piScoreBefore := GOOD_PLACE;
      piScoreAfter := BAD_PLACE;
    end;

    { words }
    ttReservedWord:
    begin
      case pcToken.Word of
        { good to break after if <exp> then, not before
         likewise case <exp> of and while <exp> dp }
        wThen, wOf, wDo:
        begin
          piScoreBefore := VERY_BAD_PLACE;
          piScoreAfter := AWESOME_PLACE;
        end;
        { in the unlikely event that one of these is embedded in a long line }
        wBegin, wEnd:
        begin
          // good to break before, even better to break after
          piScoreBefore := GOOD_PLACE;
          piScoreAfter := AWESOME_PLACE;
        end;
        wConst:
        begin
          { bad to break just after const in params as it's part of the following var
            e.g. procedure Fred(const value: integer); }
          if InFormalParams(pcToken) then
          begin
            piScoreBefore := GOOD_PLACE;
            piScoreAfter := BAD_PLACE;
          end;
        end;
      end;
    end;
    ttReservedWordDirective:
    begin
      case pcToken.Word of
        { in a property def, good to break before 'read', Ok to break before 'Write'
          bad to break just after then }
        wRead:
        begin
          if pcToken.HasParentNode(nProperty) then
          begin
            piScoreBefore := AWESOME_PLACE;
            piScoreAfter := VERY_BAD_PLACE;
          end;
        end;
        wWrite, wImplements:
        begin
          if pcToken.HasParentNode(nProperty) then
          begin
            piScoreBefore := EXCELLENT_PLACE;
            piScoreAfter := VERY_BAD_PLACE;
          end;
        end;
      end;
    end;
  end; { case }

  { slightly different rules for procedure params }
  if InFormalParams(pcToken) then
  begin
    case pcToken.Word of
      wArray, wOf:
        begin
          piScoreBefore := BAD_PLACE;
          piScoreAfter := BAD_PLACE;
        end;
        wConst, wVar, wOut:
        begin
          piScoreBefore := EXCELLENT_PLACE;
          piScoreAfter := VERY_BAD_PLACE;
        end;
    end;
  end;

  { less of a good place if it is in brackets }
  piScoreAfter := (RoundBracketLevel(pcToken) + SquareBracketLevel(pcToken)) * BRACKET_SCALE;
end;


procedure TLongLineBreaker.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
const
  DONT_BOTHER_CHARS = 2;
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  liWidth: integer;
  liLoop: integer;
  liScoreBefore, liScoreAfter: integer;
  liPlaceToBreak: integer;
  lcBreakToken: TSourceToken;
  liWidthRemaining: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  // read until the next return
  lcNext := lcSourceToken.NextToken;
  fiIndexOfFirstSolidToken := -1;
  liWidth := 0;
  lcTokens.Clear;

  while (lcNext <> nil) and (not (lcNext.TokenType in [ttReturn, ttComment])) do
  begin
    lcTokens.Add(lcNext);

    { record which token starts the line's solid text - don't want to break before it }
    if (lcNext.TokenType <> ttWhiteSpace) and (fiIndexOfFirstSolidToken = -1) then
      fiIndexOfFirstSolidToken := lcTokens.Count - 1;

    liWidth := liWidth + Length(lcNext.SourceCode);

    lcNext := lcNext.NextToken;
  end;

  // EOF or blank line means no linebreaking to do 
  if (lcNext = nil) or (lcNext = lcSourceToken) then
    exit;

  { must be solid stuff on the line }
  if fiIndexOfFirstSolidToken < 0 then
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

  { -------------
    scoring }

  { Set up scores - first the basics just for position on the line }
  lcScores.Clear;
  liWidth := 0;

  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];

    lcScores.Add(PositionScore(liLoop, liWidth) + TreeScore(lcNext));
    liWidth := liWidth + Length(lcNext.SourceCode);
  end;

  { modify the weights based on the particular source code.
    This is what will make it work -
   it is better to break line at some syntax than at other }
  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];

    ScoreToken(lcNext, liScoreBefore, liScoreAfter);

    if liLoop > 0 then
      lcScores.Items[liLoop - 1] := lcScores.Items[liLoop - 1] + liScoreBefore;
    lcScores.Items[liLoop] := lcScores.Items[liLoop] + liScoreAfter;
  end;

  { Where shall we break, if anywhere? }
  liPlaceToBreak := lcScores.IndexOfMax;

  { ignore the error conditions
   - is the break place before the first non-space token? }
  if (liPlaceToBreak < fiIndexOfFirstSolidToken) then
    exit;
  { - is it at the end of the line already, just before the existing return?}
  if (liPlaceToBreak >= (lcTokens.Count - 1)) then
    exit;

  { best breakpointis not good enough?
  if Settings.Returns.RebreakLines = rbOnlyIfGood then
  begin
    if lcScores.Items[liPlaceToBreak] < BREAK_THRESHHOLD then
      exit;
  end;
  }

  { check if the program has made a bad decision,
    e.g. the only thing on the line is a *really* long string constant and it's semicolon
    The program must break because the line is too long,
    so only place it can break lines is before the semicolon }
  lcBreakToken := lcTokens.SourceTokens[liPlaceToBreak + 1];
  liWidthRemaining := liWidth - lcBreakToken.XPosition;
  if liWidthRemaining <= DONT_BOTHER_CHARS then
    exit;

  { go break! }
  lcBreakToken.Parent.InsertChild(lcBreakToken.IndexOfSelf + 1, NewReturn); 

  { the tokens in the buffer past liPlaceToBreak now have the wrong Xpos }
  FixPos(liPlaceToBreak, lcTokens.Count - 1);
end;

end.
