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


    procedure FixPos(piStart, piEnd: integer);
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Classes,
  JclStrings,
  SourceToken, Nesting, FormatFlags, JcfSettings, SetReturns,
  TokenUtils, TokenType, ParseTreeNode, ParseTreeNodeType, WordMap;

function PositionScore(const piIndex, piIndexOfFirstSolidToken, piPos: integer): integer;
const
  NOGO_PLACE = -100; // the pits
  PLATEAU    = 100; // the baseline
  ROOFSLOPE  = 5; // the plateau slopes a bit up to a point /\
  INCREASE_TO_RIGHT_FACTOR = 15; // and it also slopes to the right
  WIDTH_SCORE_FACTOR = 5;
  TO_FAR_SCORE_FACTOR = 10;
  FAR_TO_FAR_SCORE_FACTOR = 0.3;
var
  liEffectiveWidth: integer;
  liEffectivePos: integer;
  liMidPoint: integer;
  liOneThirdPoint, liThreeQuarterPoint: integer;
  liClose:    integer;
  liOverFlow: integer;
  fUnderflow: double;
begin
  if piIndex < piIndexOfFirstSolidToken then
  begin
    Result := NOGO_PLACE;
    exit;
  end;

  { middle of the actual line (ie from first token pos to max length) }
  liEffectiveWidth := Settings.Returns.MaxLineLength - piIndexOfFirstSolidToken;
  liMidPoint       := (liEffectiveWidth div 2) + piIndexOfFirstSolidToken;
  liOneThirdPoint  := (liEffectiveWidth div 3) + piIndexOfFirstSolidToken;

  if piPos < liOneThirdPoint then
  begin
    { slope up evenly over the first third
      want a fn that at piIndexOfFirstSolidToken is NOGO_PLACE
      and slopes up evenly to PLATEAU at liMidPoint }

    { this is the distance from the first tokne  }
    liEffectivePos := piPos - piIndexOfFirstSolidToken;

    Result := NOGO_PLACE + ((PLATEAU - NOGO_PLACE) * liEffectivePos * 3) div liEffectiveWidth;
  end
  else if piPos < Settings.Returns.MaxLineLength then
  begin
    { relatively flat plateau, slight bump in the middle }
    liThreeQuarterPoint := (liEffectiveWidth * 3 div 4) + piIndexOfFirstSolidToken;
    { how close to it }
    liClose := liMidPoint - abs(piPos - liThreeQuarterPoint) + 1;
   // Assert(liClose >= 0, 'Close is neg: ' + IntToStr(liClose));

    Result := PLATEAU + (liClose * 2 div ROOFSLOPE);
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

  { general slope up to the right
    this results in the RHS slope of the plateau being favoured over the left one
  }
  if Result > NOGO_PLACE then
    Result := Result + (piPos div INCREASE_TO_RIGHT_FACTOR);
end;

function BracketScore(const pcToken: TSourceToken): integer;
const
  BRACKET_SCALE = -8;
begin
  { less of a good place if it is in brackets }
  Result := (RoundBracketLevel(pcToken) + SquareBracketLevel(pcToken)) * BRACKET_SCALE;
end;


{ experimental - score for line breaking based on the parse tree
  The idea is that higher up the tree is going to be a better place to break
  as it represents a natural break in the program flow

  larger number are better so invert

function TreeScore(const pcToken: TSourceToken): integer;
const
  DEPTH_FACTOR = 5;
  FIRST_CHILD_FACTOR = 20;
begin
  Result := - (pcToken.Level * DEPTH_FACTOR);
  if pcToken.IndexOfSelf = 0 then
    Result := Result + FIRST_CHILD_FACTOR;
end;
}


{ want to capture the effect that that breaking the line near the end
  (ie with few chars to go) is bad.
  e.g. max allowed line length is 100, actual line is 102 chars long
  breaking at char 101, just before the semicolon, would really suck

  Have used a factor that breaking 15 or more chars from the end
  incurs no penalty, Penalty is not linear:
  It is insignificant for 15, 14, chars from end,
  getting sizeable from 10 down to 5 spaces to the end
  and very large for 1,2 chars to end

  The function that I have used is Ceil((x ^ 2) / 2)
  where x is (15 - <spaces to end>)
}
function NearEndScore(const piSpacesToEnd: integer): integer;
const
  TAIL_SIZE: integer = 15;

  penalties: array[1..15] of integer = (
    113, 98, 85, 72, 61,
     50, 41, 32, 25, 18,
     13,  8,  5,  2,  1);
begin
  Assert(piSpacesToEnd >= 0);
  if piSpacesToEnd > TAIL_SIZE then
  begin
    Result := 0;
  end
  else
  begin
    Result := penalties[piSpacesToEnd];
  end;
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
  EXCELLENT_PLACE = 30;
  AWESOME_PLACE   = 40;
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
end;




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


procedure TLongLineBreaker.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
const
  GOOD_BREAK_THRESHHOLD = 50;
  ANY_BREAK_THRESHHOLD = -10;
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  liWidth: integer;
  liIndexOfFirstSolidToken: integer;
  liLoop: integer;
  liScoreBefore, liScoreAfter: integer;
  liPlaceToBreak: integer;
  lcBreakToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType <> ttReturn then
    exit;

  // read until the next return
  lcNext := lcSourceToken.NextToken;
  liIndexOfFirstSolidToken := -1;
  liWidth := 0;
  lcTokens.Clear;

  while (lcNext <> nil) and (not (lcNext.TokenType in [ttReturn, ttComment])) do
  begin
    lcTokens.Add(lcNext);

    { record which token starts the line's solid text - don't want to break before it }
    if (lcNext.TokenType <> ttWhiteSpace) and (liIndexOfFirstSolidToken = -1) then
      liIndexOfFirstSolidToken := lcTokens.Count - 1;

    liWidth := liWidth + Length(lcNext.SourceCode);

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

  { -------------
    scoring }

  { Set up scores - first the basics just for position on the line }
  lcScores.Clear;
  liWidth := 0;

  (* better coded this way
  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];
    liWidth := liWidth + Length(lcNext.SourceCode);

    { thse scores are simply property of one token }
    liScoreAfter := PositionScore(liLoop, liIndexOfFirstSolidToken, liWidth) +
      TreeScore(lcNext) + BracketScore(lcNext);
    lcScores.Add(liScoreAfter);
  end;  *)

  // easier to debug *this* way
  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];
    liWidth := liWidth + Length(lcNext.SourceCode);

    { thse scores are simply property of one token }
    liScoreAfter := PositionScore(liLoop, liIndexOfFirstSolidToken, liWidth);
    lcScores.Add(liScoreAfter);
  end;

  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];
    liScoreAfter := NearEndScore(liWidth - lcNext.XPosition);
    // subtract this one - bad to break near end
    lcScores.Items[liLoop] := lcScores.Items[liLoop] - liScoreAfter;
  end;

  for liLoop := 0 to lcTokens.Count - 1 do
  begin
    lcNext := lcTokens.SourceTokens[liLoop];
    liScoreAfter := BracketScore(lcNext);
    lcScores.Items[liLoop] := lcScores.Items[liLoop] + liScoreAfter;
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
  if (liPlaceToBreak < liIndexOfFirstSolidToken) then
    exit;
  { - is it at the end of the line already, just before the existing return?}
  if (liPlaceToBreak >= (lcTokens.Count - 1)) then
    exit;

  { best breakpointis not good enough? }
  if Settings.Returns.RebreakLines = rbOnlyIfGood then
  begin
    if lcScores.Items[liPlaceToBreak] < GOOD_BREAK_THRESHHOLD then
      exit;
  end
  else
  begin
    Assert(Settings.Returns.RebreakLines = rbUsually);
    if lcScores.Items[liPlaceToBreak] < ANY_BREAK_THRESHHOLD then
      exit;
  end;


  { check if the program has made a bad decision,
    e.g. the only thing on the line is a *really* long string constant and it's semicolon
    The program must break because the line is too long,
    so only place it can break lines is before the semicolon }
  lcBreakToken := lcTokens.SourceTokens[liPlaceToBreak];

  { go break! }
  lcBreakToken.Parent.InsertChild(lcBreakToken.IndexOfSelf + 1, NewReturn); 

  { the tokens in the buffer past liPlaceToBreak now have the wrong Xpos }
  FixPos(liPlaceToBreak, lcTokens.Count - 1);
end;


(*
  //TEST code used to get a graph of the position scoring function into excel

var
  liLoop, liScore: integer;
  lsData: string;
const
  INDEX_OF_FIRST = 10;
  MAX_TOKENS = 100;
initialization
  Settings.Returns.MaxLineLength  := MAX_TOKENS;

  { debug temp to graph the pos fn }
  for liLoop := 0 to (MAX_TOKENS * 3 div  2) do
  begin
    liScore := PositionScore(liLoop, INDEX_OF_FIRST, liLoop);

    lsData := lsData + IntToStr(liScore) + ', ';
  end;

  StringToFile('C:\temp\posvalues.txt', lsData)

*)
end.
