unit BasicStats;

{
  AFS 25 March 03
  Basic stats on the unit
  Proof of concept to do crude code analysis on the unit

  Since it doesn't alter anything and wants to read the entire unit
  it is not a switchable visitor }

interface

uses BaseVisitor, VisitParseTree;


type
  TBasicStats = class(TBaseTreeNodeVisitor)
  private
    fiTotalTokens, fiTotalChars: integer;

    // tokens can be divided into 3 categories - comments, spaces&rets, code
    fiSpaceTokens, fiCommentTokens, fiSolidTokens: integer;
    fiSpaceChars, fiCommentChars, fiSolidChars: integer;

    fiLines: integer;
  protected
  public
    procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    function FinalSummary(var psMessage: string): Boolean; override;
  end;

implementation

uses
  { delphi }
  SysUtils,
  JclStrings,
  { JCF  }
  SourceToken, TokenType;

function DisplayFloat(const ex: extended): string;
begin
  Result := FloatToStrF(ex, ffNumber, 9, 2);
end;

function DisplayRatio(const exNum, exDenom: extended): string;
begin
  if exDenom = 0 then
    Result := '-'
  else
    Result := DisplayFloat(exNum / exDenom);
end;

function DisplayPercent(const exNum, exDenom: extended): string;
begin
  if exDenom = 0 then
    Result := '-'
  else
    Result := DisplayFloat(exNum * 100 / exDenom) + '%';
end;


procedure TBasicStats.VisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  liLen: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  inc (fiTotalTokens);
  liLen := Length(lcSourceToken.SourceCode);
  fiTotalChars := fiTotalChars + liLen;

  case lcSourceToken.TokenType of
    ttComment:
    begin
      inc(fiCommentTokens);
      fiCommentChars := fiCommentChars + liLen;

      fiLines := fiLines + StrStrCount(lcSourceToken.SourceCode, AnsiLineBreak);
    end;
    ttReturn:
    begin
      inc(fiLines);
      inc(fiSpaceTokens);
      fiSpaceChars := fiSpaceChars + liLen;
    end;
    ttWhiteSpace:
    begin
      inc(fiSpaceTokens);
      fiSpaceChars := fiSpaceChars + liLen;
    end;
    else
    begin
      inc(fiSolidTokens);
      fiSolidChars := fiSolidChars + liLen;
    end;
  end;
end;

function TBasicStats.FinalSummary(var psMessage: string): Boolean;
begin
  Result := True;

  psMessage := AnsiLineBreak + 'Basic numbers and averages: ' + AnsiLineBreak +
    'Unit is ' + IntToStr(fiLines) + ' lines long' +  AnsiLineBreak +
    'Unit has ' + IntToStr(fiTotalTokens) + ' tokens in '  +
    IntToStr(fiTotalChars) + ' characters: ' +  AnsiLineBreak +
    DisplayRatio(fiTotalChars, fiTotalTokens) + ' chars per token' + AnsiLineBreak +
    DisplayRatio(fiTotalChars, fiLines) + ' chars per line ' +  AnsiLineBreak +
    DisplayRatio(fiTotalTokens, fiLines) + ' tokens per line ' +  AnsiLineBreak + AnsiLineBreak +

    IntToStr(fiCommentTokens) + ' comments in ' + IntToStr(fiCommentChars) + ' characters ' + AnsiLineBreak +
    DisplayRatio(fiCommentChars, fiCommentTokens) + ' chars per comment' + AnsiLineBreak +
    DisplayPercent(fiCommentChars, fiTotalChars) + ' of chars are comments ' + AnsiLineBreak + AnsiLineBreak +

    IntToStr(fiSpaceTokens) + ' spacing and return tokens in ' + IntToStr(fiSpaceChars) + ' characters ' +   AnsiLineBreak +
    DisplayRatio(fiSpaceChars, fiSpaceTokens) + ' chars per token' + AnsiLineBreak +
    DisplayPercent(fiSpaceChars, fiTotalChars) + ' of chars are spacing ' + AnsiLineBreak + AnsiLineBreak +

    IntToStr(fiSolidTokens) + ' solid tokens in ' + IntToStr(fiSolidChars) + ' characters ' +  AnsiLineBreak +
    DisplayRatio(fiSolidChars, fiSolidTokens) + ' chars per token' + AnsiLineBreak +
    DisplayPercent(fiSolidChars, fiTotalChars) + ' of chars are solid' + AnsiLineBreak +
    DisplayPercent(fiSolidTokens, fiTotalTokens) + ' of tokens are solid';

end;

end.
