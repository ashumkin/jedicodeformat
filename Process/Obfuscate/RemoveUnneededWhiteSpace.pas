unit RemoveUnneededWhiteSpace;

{ AFS 29 December 2002
  Obfuscation visitor
  This is to completely remove white space
  when it is not needed, e.g. turn "a := a + 1 ;" to "a:=a+1;"
}

interface

uses BaseVisitor, VisitParseTree;

type
  TRemoveUnneededWhiteSpace = class(TBaseTreeNodeVisitor)
    public
      procedure VisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses
  JclStrings,
  { local } SourceToken, TokenType;

function TextOrNumberString(const str: string): boolean;
var
  liLoop: integer;
  ch:     char;
begin
  Result := True;

  for liLoop := 1 to Length(str) do
  begin
    ch := str[liLoop];
    if not (CharIsAlphaNum(ch) or (ch = '_') or (ch = '.')) then
    begin
      Result := False;
      break;
    end;
  end;
end;

function OnlyOneText(const pt1, pt2: TSourceToken): boolean;
var
  b1, b2: boolean;
begin
  b1 := TextOrNumberString(pt1.SourceCode);
  b2 := TextOrNumberString(pt2.SourceCode);

  { one or the other, not both }
  Result := b1 xor b2;
end;



const
  MiscUnspacedTokens: TTokenTypeSet =
    [ttLiteralString, ttSemiColon, ttColon, ttComma, ttDot, ttAssign, ttReturn];


function NeedSpaceBetween(const pt1, pt2: TSourceToken): boolean;
begin
  Result := True;

  if ((pt1 = nil) or (pt2 = nil)) then
  begin
    Result := False;
    exit;
  end;

  { never need a space next to a bracket }
  if (pt1.TokenType in BracketTokens) or (pt2.TokenType in BracketTokens) then
  begin
    Result := False;
    exit;
  end;

  { or dot or comma etc }
  if (pt1.TokenType in MiscUnspacedTokens) or (pt2.TokenType in MiscUnspacedTokens) then
  begin
    Result := False;
    exit;
  end;

  { don't need white space next to white space }
  if (pt1.TokenType = ttWhiteSpace) or (pt2.TokenType = ttWhiteSpace) then
  begin
    Result := False;
    exit;
  end;

  { if one token is text, and the other not, don't need white space
   for this numbers count as text, for e.g.
   "for liLoop := 0to3do" is not valid, neither is "for liLoop := 0 to3 do",
   must be for liLoop := 0 to 3 do
   }

  if (pt1.TokenType in TextOrNumberTokens) and not (pt2.TokenType in TextOrNumberTokens)
    then
  begin
    Result := False;
    exit;
  end;

  {operators such as '<', '='  are counted as texual tokens
   no space betwen such an operator and a different token

   (unlike operators like 'in', 'is' which need a space between them & other words
  }


  if ((pt1.TokenType = ttOperator) or (pt2.TokenType = ttOperator)) and
    (pt1.TokenType <> pt2.TokenType) then
  begin
    if OnlyOneText(pt1, pt2) then
    begin
      Result := False;
      exit;
    end;
  end;
end;



procedure TRemoveUnneededWhiteSpace.VisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSpace, lcBefore, lcAfter: TSourceToken;
begin
  { this visitor needs to operate on the white space token
    depending on what comes before and after it
  }
  lcSpace := TSourceToken(pcNode);
  if lcSpace.TokenType <> ttWhiteSpace then
    exit;

  lcBefore := lcSpace.PriorToken;
  lcAfter := lcSPace.NextToken;

  if not NeedSpaceBetween(lcBefore, lcAfter) then
    prVisitResult.Action := aDelete;
end;

end.
