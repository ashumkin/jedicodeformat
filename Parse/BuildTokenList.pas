{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Tokeniser.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

unit BuildTokenList;

{ AFS 29 Nov 1999
 converts the input string of chars into a list of tokens
 This is the lexical analysis phase of the parsing
}

interface

uses
  CodeReader, Tokens, SourceToken, SourceTokenList;

type

  TBuildTokenList = class(TObject)
  private

    { property implementation }
    fcReader: TCodeReader;

      { implementation of GetNextToken }
    function TryReturn(const pcToken: TSourceToken): boolean;

    function TryCurlyComment(const pcToken: TSourceToken): boolean;
    function TrySlashComment(const pcToken: TSourceToken): boolean;
    function TryBracketStarComment(const pcToken: TSourceToken): boolean;

    function TryWhiteSpace(const pcToken: TSourceToken): boolean;
    function TryLiteralString(const pcToken: TSourceToken): boolean;
    function TryDoubleQuoteLiteralString(const pcToken: TSourceToken): boolean;

    function TryNumber(const pcToken: TSourceToken): boolean;
    function TryHexNumber(const pcToken: TSourceToken): boolean;

    function TryDots(const pcToken: TSourceToken): boolean;

    function TryAssign(const pcToken: TSourceToken): boolean;


    function TrySingleCharToken(const pcToken: TSourceToken): boolean;

    function TryPunctuation(const pcToken: TSourceToken): boolean;
    function TryWord(const pcToken: TSourceToken): boolean;

    function GetNextToken: TSourceToken;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    function BuildTokenList: TSourceTokenList;

    property Reader: TCodeReader Read fcReader Write fcReader;
  end;


implementation

uses
    { delphi }Forms,
    { jcl }JclStrings;

{ TBuildTokenList }

constructor TBuildTokenList.Create;
begin
  inherited;
  fcReader := nil;
end;

destructor TBuildTokenList.Destroy;
begin
  inherited;
end;

function TBuildTokenList.GetNextToken: TSourceToken;
var
  lcNewToken: TSourceToken;

  procedure DoAllTheTries;
  begin
    { first look for return }
    if TryReturn(lcNewToken) then
      exit;
    { comments }
    if TryCurlyComment(lcNewToken) then
      exit;
    if TrySlashComment(lcNewToken) then
      exit;
    if TryBracketStarComment(lcNewToken) then
      exit;
    { the rest }
    if TryWhiteSpace(lcNewToken) then
      exit;
    if TryLiteralString(lcNewToken) then
      exit;
    if TryDoubleQuoteLiteralString(lcNewToken) then
      exit;

    if TryWord(lcNewToken) then
      exit;
    if TryNumber(lcNewToken) then
      exit;
    if TryHexNumber(lcNewToken) then
      exit;

    if TryDots(lcNewToken) then
      exit;

    { attempt assign before colon }
    if TryAssign(lcNewToken) then
      exit;

    if TryPunctuation(lcNewToken) then
      exit;

    if TrySingleCharToken(lcNewToken) then
      exit;

    { default }
    lcNewToken.TokenType  := ttUnknown;
    lcNewToken.SourceCode := Reader.Current;
    Reader.Consume(1);
  end;

begin
  if Reader.EndOfFile then
    Result := nil
  else
  begin
    Reader.BufferLength := 1;
    lcNewToken := TSourceToken.Create;
    DoAllTheTries;

    lcNewToken.WordType := WordTypeOfToken(lcNewToken.TokenType);
    Result := lcNewToken;
  end;
end;

{-------------------------------------------------------------------------------
  worker fns for GetNextComment }

function TBuildTokenList.TryBracketStarComment(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  if not (Reader.Current = '(') then
    exit;

  Reader.BufferLength := 2;

  if Reader.BufferCharsLeft(2) <> '(*' then
    exit;

  { if the comment starts with (*) that is not the end of the comment }
  Reader.IncBuffer;
  Reader.IncBuffer;

  { until *) or End of file }
  while (Reader.BufferCharsRight(2) <> '*)') and ( not Reader.BufferEndOfFile) do
    Reader.IncBuffer;

  pcToken.TokenType := ttComment;
  pcToken.CommentStyle := eBracketStar;
  pcToken.SourceCode := Reader.ConsumeBuffer;
  Result := True;
end;


function TBuildTokenList.TryCurlyComment(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  if Reader.Current <> '{' then
    exit;

  pcToken.TokenType  := ttComment;
  pcToken.SourceCode := Reader.Current;
  Reader.Consume;

  { compiler directive are the comments with a $ just after the open-curly
    this is always the case }
  if Reader.Current = '$' then
    pcToken.CommentStyle := eCompilerDirective
  else
    pcToken.CommentStyle := eCurlyBrace;

  { comment is ended by close-curly or by EOF (bad source) }
  while (Reader.Last <> '}') and not (Reader.BufferEndOfFile) do
    Reader.IncBuffer;

  pcToken.SourceCode := pcToken.SourceCode + Reader.ConsumeBuffer;
  Result := True;
end;

function TBuildTokenList.TrySlashComment(const pcToken: TSourceToken): boolean;
var
  liLen: integer;
begin
  Result := False;
  if Reader.Current <> '/' then
    exit;

  Reader.BufferLength := 2;

  { until end of line or file }
  if Reader.BufferCharsLeft(2) <> '//' then
    exit;

  while ( not CharIsReturn(Reader.Last)) and ( not Reader.BufferEndOfFile) do
    Reader.IncBuffer;

  liLen := Reader.BufferLength;
  if CharIsReturn(Reader.Last) then
    Dec(liLen);
  Reader.BufferLength := liLen;

  pcToken.TokenType := ttComment;
  pcToken.CommentStyle := eDoubleSlash;
  pcToken.SourceCode := Reader.ConsumeBuffer;
  Result := True;
end;


function TBuildTokenList.TryReturn(const pcToken: TSourceToken): boolean;
var
  chNext: char;
begin
  Result := False;
  if not CharIsReturn(Reader.Current) then
    exit;

  pcToken.TokenType  := ttReturn;
  pcToken.SourceCode := Reader.Current;
  Reader.Consume;

  { concat the next return char if it is not the same
    This will recognise <cr><lf> or <lf><cr>, but not <cr><cr> }

  chNext := Reader.Current;
  if CharIsReturn(chNext) and (chNext <> pcToken.SourceCode[1]) then
  begin
    pcToken.SourceCode := pcToken.SourceCode + chNext;
    Reader.Consume;
  end;
  Result := True;
end;

{ have to combine this with literal chars, as these can be mixed,
  eg 'Hello'#32'World' and #$12'Foo' are both valid strings }
function TBuildTokenList.TryLiteralString(const pcToken: TSourceToken): boolean;

  function TrySubString(const pcToken: TSourceToken): boolean;
  var
    lbFoundEnd: boolean;
    liLen: integer;
    lCh: char;
  begin
    Result := False;

    if Reader.Current <> AnsiSingleQuote then
      exit;

    { find the end of the string
      string is ended by another quote char or by return (bad source)
      but two consecutive quote chars can occur without ending the string }

    lbFoundEnd := False;

    while not lbFoundEnd do
    begin
      if not lbFoundEnd then
        Reader.IncBuffer;

      { error - end on line end or EOF }
      lCh := Reader.Last;
      if CharIsReturn(lCh) then
        lbFoundEnd := True;
      if Reader.BufferEndOfFile then
        lbFoundEnd := True;

      if (Reader.Last = AnsiSingleQuote) then
      begin
        { followed by another? }
        Reader.IncBuffer;
        if (Reader.Last <> AnsiSingleQuote) then
          lbFoundEnd := True;
      end;
    end; { while not found }

    liLen := StrLastPos(AnsiSingleQuote, Reader.Buffer);
    if liLen < 0 then
    begin
      liLen := Reader.BufferLength;
      if CharIsReturn(Reader.Last) then
        Dec(liLen);
    end;

    Assert(liLen > 0, 'Bad string' + StrDoubleQuote(Reader.Buffer));
    Reader.BufferLength := liLen;

    pcToken.SourceCode := pcToken.SourceCode + Reader.ConsumeBuffer;
    Result := True;
  end;

  function TryHatLiteralChar(const pcToken: TSourceToken): boolean;
  begin
    Result := False;
    if Reader.Current <> '^' then
      exit;

    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;

    { can be any single char A-Z and symbols }
    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end;


  function TryLiteralChar(const pcToken: TSourceToken): boolean;
  begin
    Result := False;
    if Reader.Current <> '#' then
      exit;

    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;

    { can be hex string, as in #$F }
    if Reader.Current = '$' then
    begin
      { eat the $ }
      pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
      Reader.Consume;

      { hexidecimal string - concat any subsequent digits }
      while (Reader.Current in AnsiHexDigits) do
      begin
        pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
        Reader.Consume;
        Result := True;
      end;
    end
    else
    begin
      { normal decimal string - concat any subsequent digits }
      while CharIsDigit(Reader.Current) do
      begin
        pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
        Reader.Consume;
        Result := True;
      end;
    end;
  end;

var
  lbHatCharConstant: Boolean;
begin
  Result := False;
  lbHatCharConstant := False;

  if Reader.Current = '^' then
  begin
    { compound char const is, for e.g. ^M'foo' or ^M^N or ^M#13 }
    if Reader.BufferChar(2) in ['^', AnsiSingleQuote, '#'] then
      lbHatCharConstant := True;
  end;

  if (Reader.Current in ['#', AnsiSingleQuote]) or lbHatCharConstant then
  begin
    { read any sequence of literal chars , eg #$F or #32#32 or even #27#$1E }
    while Reader.Current in ['#', AnsiSingleQuote, '^'] do
    begin
      if Reader.Current = '#' then
      begin
        if TryLiteralChar(pcToken) then
          Result := True;
      end
      else if Reader.Current = AnsiSingleQuote then
      begin
        if TrySubString(pcToken) then
          Result := True;
      end
      else if Reader.Current = '^' then
      begin
        if TryHatLiteralChar(pcToken) then
          Result := True;
      end;
    end;

    if Result then
      pcToken.TokenType := ttLiteralString;
  end;
end;

{ these can happen inside asm
  not going to give them the same degree of sophistication though }
function TBuildTokenList.TryDoubleQuoteLiteralString(
  const pcToken: TSourceToken): boolean;
begin
  Result := False;

  if Reader.Current = AnsiDoubleQuote then
  begin
    Result := True;

    repeat
      pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
      Reader.Consume;

    until Reader.Current = AnsiDoubleQuote;

    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;

    pcToken.TokenType := ttLiteralString;
  end;
end;

function TBuildTokenList.TryWord(const pcToken: TSourceToken): boolean;

  function IsWordChar(const ch: char): boolean;
  begin
    Result := CharIsAlpha(ch) or (ch = '_');
  end;

begin
  Result := False;

  if not IsWordChar(Reader.Current) then
    exit;

  pcToken.SourceCode := Reader.Current;
  Reader.Consume;

  { concat any subsequent word chars }
  while IsWordChar(Reader.Current) or CharIsDigit(Reader.Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end;

  { try to recognise the word as built in }
  pcToken.TokenType := TypeOfToken(pcToken.SourceCode);
  if pcToken.TokenType = ttUnknown then
    pcToken.TokenType := ttIdentifier;

  Result := True;
end;

function CharIsWhiteSpaceNoReturn(const ch: AnsiChar): boolean;
begin
  Result := CharIsWhiteSpace(ch) and (ch <> AnsiLineFeed) and (ch <> AnsiCarriageReturn);
end;

function TBuildTokenList.TryWhiteSpace(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  if not CharIsWhiteSpaceNoReturn(Reader.Current) then
    exit;

  pcToken.TokenType  := ttWhiteSpace;
  pcToken.SourceCode := Reader.Current;
  Reader.Consume;

  { concat any subsequent return chars }
  while CharIsWhiteSpaceNoReturn(Reader.Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end;

  Result := True;
end;

function TBuildTokenList.TryAssign(const pcToken: TSourceToken): boolean;
begin
  Result := False;

  if Reader.Current <> ':' then
    exit;

  Reader.BufferLength := 2;

  if Reader.Buffer <> ':=' then
    exit;

  pcToken.TokenType := ttAssign;
  pcToken.SourceCode := Reader.ConsumeBuffer;
  Result := True;
end;

function TBuildTokenList.TryNumber(const pcToken: TSourceToken): boolean;
var
  lbHasDecimalSep: boolean;
begin
  Result := False;

  { recognise a number -
   they don't start with a '.' but may contain one

   a minus sign in front is considered unary operator not part of the number
   this is bourne out by the compiler considering
    '- 0.3' and -0.3' to be the same value
    and -.3 is not legal at all }

  { first one must be a digit }
  if not CharIsDigit(Reader.Current) then
    exit;

  if (Reader.Current = '.') or (Reader.Current = '-') then
    exit;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Reader.Current;
  Reader.Consume;
  lbHasDecimalSep := False;

  { concat any subsequent number chars
    only one decimal seperator allowed

    also NB that two dots can be array range, as in
    var foo = array[1..100] of integer;
    ie one dat = decimal
    two dots = end of number
  }
  while CharIsDigit(Reader.Current) or (Reader.Current = '.') do
  begin
    // have we got to the dot?
    if (Reader.Current = '.') then
    begin
      if Reader.BufferCharsLeft(2) = '..' then
        break;

      if lbHasDecimalSep then
        // oops! a second one
        break
      else
        lbHasDecimalSep := True;
    end;

    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end;

  { scientific notation suffix, eg 3e2 = 30, 2.1e-3 = 0.0021 }

  { check for a trailing 'e' }
  if Reader.Current in ['e', 'E'] then
  begin
    // sci notation mode
    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;

    // can be a minus or plus here
    if Reader.Current in ['-', '+'] then
    begin
      pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
      Reader.Consume;
    end;

    { exponent must be integer }
    while CharIsDigit(Reader.Current) do
    begin
      pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
      Reader.Consume;
    end;
  end;

  Result := True;
end;

{ NB: do not localise '.' with DecimalSeperator
  Delphi source code does *not* change for this }
function TBuildTokenList.TryHexNumber(const pcToken: TSourceToken): boolean;
var
  lbHasDecimalSep: boolean;
begin
  Result := False;

  { starts with a $ }
  if Reader.Current <> '$' then
    exit;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Reader.Current;
  Reader.Consume;
  lbHasDecimalSep := False;

  { concat any subsequent number chars }
  while (Reader.Current in AnsiHexDigits) or (Reader.Current = '.') do
  begin
    // have we got to the dot?
    if (Reader.Current = '.') then
    begin
      if Reader.BufferCharsLeft(2) = '..' then
        break;

      if lbHasDecimalSep then
        // oops! a second one
        break
      else
        lbHasDecimalSep := True;
    end;

    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end;

  Result := True;
end;

{ try the range '..' operator and object access  '.' operator }
function TBuildTokenList.TryDots(const pcToken: TSourceToken): boolean;
begin
  Result := False;

  if Reader.Current <> '.' then
    exit;

  pcToken.SourceCode := Reader.Current;
  Reader.Consume;

  if Reader.Current = '.' then
  begin
    pcToken.TokenType  := ttDoubleDot;
    pcToken.SourceCode := pcToken.SourceCode + Reader.Current;
    Reader.Consume;
  end
  else
  begin
    pcToken.TokenType := ttDot;
  end;

  Result := True;
end;


function IsPuncChar(const ch: char): boolean;
begin
  Result := False;

  if CharIsWhiteSpace(ch) then
    exit;
  if CharIsAlphaNum(ch) then
    exit;
  if CharIsReturn(ch) then
    exit;

  if CharIsControl(ch) then
    exit;

  Result := True;
end;

function TBuildTokenList.TryPunctuation(const pcToken: TSourceToken): boolean;


  function FollowsPunctuation(const chLast, ch: char): boolean;
  const
    { these have meanings on thier own and should not be recognised as part of the punc.
     e.g '=(' is not a punctation symbol, but 2 of them ( for e.g. in const a=(3);
     simlarly ');' is 2 puncs }
    UnitaryPunctuation: set of char = [
      AnsiSingleQuote, '"', '(', ')', '[', ']', '{',
      '#', '$', '_', ';', '@', '^', ','];

   { these can't have anything following them:
    for e.g, catch the case if a=-1 then ...
      where '=-1' should be read as '=' '-1' not '=-' '1'
      Nothing legitimately comes after '=' AFAIK
      also a:=a*-1;
      q:=q--1; // q equals q minus minus-one. It sucks but it compiles so it must be parsed
      etc }
    SingleChars: set of char = ['=', '+', '-', '*', '/', '\'];

  begin
    Result := False;

    if (chLast in UnitaryPunctuation) or (ch in UnitaryPunctuation) then
      exit;

    if chLast in SingleChars then
      exit;

    { '<' or '<' can only be followed by '<', '>' or '='.
     Beware of "if x<-1" }
    if (chLast in ['<', '>']) and not (ch in ['<', '>', '=']) then
      exit;

    // ':' can be followed by '='
    if (chLast = ':') and (ch <> '=') then
      exit;

    Result := IsPuncChar(ch);
  end;

var
  leWordType:  TWordType;
  leTokenType: TTokenType;
  lcLast:      char;
begin
  Result := False;

  if not IsPuncChar(Reader.Current) then
    exit;

  pcToken.TokenType := ttPunctuation;
  lcLast := Reader.Current;
  pcToken.SourceCode := lcLast;
  Reader.Consume;

  { concat any subsequent punc chars }
  while FollowsPunctuation(lcLast, Reader.Current) do
  begin
    lcLast := Reader.Current;
    pcToken.SourceCode := pcToken.SourceCode + lcLast;
    Reader.Consume;
  end;

  { try to recognise the punctuation as an operator }
  TypeOfToken(pcToken.SourceCode, leWordType, leTokenType);
  if leTokenType <> ttUnknown then
  begin
    pcToken.TokenType := leTokenType;
  end;

  Result := True;
end;

function TBuildTokenList.TrySingleCharToken(const pcToken: TSourceToken): boolean;
begin
  Result := False;

  pcToken.TokenType := TypeOfToken(Reader.Current);
  if pcToken.TokenType <> ttUnknown then
  begin
    pcToken.SourceCode := Reader.Current;
    Reader.Consume;
    Result := True;
  end;
end;

function TBuildTokenList.BuildTokenList: TSourceTokenList;
const
  UPDATE_INTERVAL = 4096; // big incre,ents here, this is fatser than parsing
var
  lcList:    TSourceTokenList;
  lcNew:     TSourceToken;
  liCounter: integer;
begin
  liCounter := 0;
  lcList    := TSourceTokenList.Create;

  while not Reader.EndOfFile do
  begin
    lcNew := GetNextToken;
    lcList.Add(lcNew);

    Inc(liCounter);
    if (liCounter mod UPDATE_INTERVAL) = 0 then
      Application.ProcessMessages;
  end;

  Result := lcList;
end;


end.
