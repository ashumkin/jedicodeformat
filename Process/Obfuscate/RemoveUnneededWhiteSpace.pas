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
  { local } SourceToken, TokenType, WordMap, ParseTreeNodeType;

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


const
  MiscUnspacedTokens: TTokenTypeSet = [
    ttLiteralString, ttSemiColon, ttColon, ttComma,
    ttDot, ttDoubleDot, ttAssign, ttReturn];


function NeedSpaceBetween(const pt1, pt2: TSourceToken): boolean;
begin
  Result := True;

  if ((pt1 = nil) or (pt2 = nil)) then
  begin
    Result := False;
    exit;
  end;

  { need to keep space before ASM @@ thingy}
  if (pt2.Word = wAtSign) and pt2.HasParentNode(nAsmStatement) then
    exit;

  { never need a space next to a bracket }
  if (pt1.TokenType in BracketTokens) or (pt2.TokenType in BracketTokens) then
  begin
    Result := False;
    exit;
  end;

  { never need space around semicolon }
  if (pt1.TokenType = ttSemiColon) or (pt2.TokenType = ttSemiColon) then
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

  if (pt1.TokenType in TextOrNumberTokens) and (pt2.Word = wAtSign) and (pt1.HasParentnode(nAsm)) then
  begin
    Result := False;
    exit;
  end;

  { if one token is text or number, and the other not, don't need white space
   for this numbers count as text, for e.g.
   "for liLoop := 0to3do" is not valid, neither is "for liLoop := 0 to3 do",
   must be for liLoop := 0 to 3 do
   }

  if TextOrNumberString(pt1.SourceCode) then
  begin
    { always space between two text/number tokens }
    Result := TextOrNumberString(pt2.SourceCode);
  end
  else
    Result := False;
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
  if lcBefore = nil then
    exit;

  lcAfter := lcSpace.NextToken;
  if lcAfter = nil then
    exit;

  if not NeedSpaceBetween(lcBefore, lcAfter) then
    prVisitResult.Action := aDelete;
end;

end.
