unit SingleSpaceAfter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SingleSpaceAfter, released May 2003.
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

interface

{ AFS 9 Dec 1999
  Single space after : }

uses SwitchableVisitor, VisitParseTree;


type
  TSingleSpaceAfter = class(TSwitchableVisitor)
    private
    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;


implementation

uses
  JclStrings,
  JcfMiscFunctions,
  SourceToken, Tokens, ParseTreeNodeType, JcfSettings,
  FormatFlags, TokenUtils;

  const
  SingleSpaceAfterTokens: TTokenTypeSet = [ttColon, ttAssign, ttComma];

  SingleSpaceAfterWords: TTokenTypeSet = [
    ttProcedure, ttFunction,
    ttConstructor, ttDestructor, ttProperty,
    ttOf, ttDo, ttWhile, ttUntil, ttCase, ttIf, ttTo, ttDownTo,

    // some unary operators
    ttNot,
    // all operators that are always binary
    ttAnd, ttAs, ttDiv, ttIn, ttIs, ttMod, ttOr, ttShl, ttShr, ttXor,
    ttTimes, ttFloatDiv, ttEquals, ttGreaterThan, ttLessThan,
    ttGreaterThanOrEqual, ttLessThanOrEqual, ttNotEqual];

  PossiblyUnaryOperators: TTokenTypeSet = [ttPlus, ttMinus];

function NeedsSingleSpace(const pt, ptNext: TSourceToken): boolean;
begin
  Assert(pt <> nil);
  Assert(ptNext <> nil);

  Result := False;

  if pt.HasParentNode(nAsm) then
    exit;

  // if the next token is a comment, leave it where it is, do not adjust spacing
  if ptNext.TokenType = ttComment then
    exit;

  // semicolons
  if (pt.TokenType = ttSemiColon) then
  begin

    { semciolon as a record field seperator in a const record declaration
     has no newline (See ReturnAfter.pas), just a single space }
    if (pt.HasParentNode(nRecordConstant)) then
    begin
      Result := True;
      exit;
    end;

    { semicolon  in param  declaration list }
    if (pt.HasParentNode(nFormalParams)) then
    begin
      Result := True;
      exit;
    end;

    { semicolon in param lists in proc type def. as above }
    if (pt.HasParentNode(nProcedureType)) then
    begin
      Result := True;
      exit;
    end;

    { semicolon in procedure directives }
    if (pt.HasParentNode(nProcedureDirectives)) then
    begin
      Result := True;
      exit;
    end;

  end;// semicolon

  { function foo: integer; has single space after the colon
    single space after colon - anywhere? }
  if pt.TokenType = ttColon then
  begin
    Result := True;
  end;

  if (pt.TokenType in SingleSpaceAfterTokens) then
  begin
    Result := True;
    exit;
  end;

  { 'absolute' as a var directive }
  if (pt.TokenType = ttAbsolute) and pt.HasParentNode(nAbsoluteVar) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType in SingleSpaceAfterWords) then
  begin
    { 'procedure' and 'function' in proc type def don't have space after, e.g.
      type
        TFredProc = procedure(var psFred: integer); }

    if (pt.HasParentNode(nProcedureType, 2)) and (ptNext.TokenType in [ttOpenBracket, ttSemiColon]) then
      Result := False
    else
      Result := True;

    exit;
  end;

  { + or - but only if it is a binary operator, ie a term to the left of it }
  if (pt.TokenType in PossiblyUnaryOperators) and (pt.HasParentNode(nExpression)) and
    (not IsUnaryOperator(pt)) then
  begin
    Result := True;
    exit;
  end;

  { only if it actually is a directive, see TestCases/TestBogusDirectives for details }
  if (pt.TokenType in AllDirectives) and (pt.HasParentNode(DirectiveNodes)) and
    (ptNext.TokenType <> ttSemiColon) then
  begin
    Result := True;
    exit;
  end;

  if pt.TokenType = ttEquals then
  begin
    Result := True;
    exit;
  end;

  { 'in' in the uses clause }
  if (pt.TokenType = ttIn) and (pt.HasParentNode(nUses)) then
  begin
    Result := True;
    exit;
  end;

  { const or var as parameter var types }
  if (pt.TokenType in ParamTypes) and (pt.HasParentNode(nFormalParams)) then
  begin
    // beware of 'procedure foo (bar: array of const);' and the like
    if not ((pt.TokenType = ttConst) and pt.HasParentNode(nType, 1)) then
    begin
      Result := True;
      exit;
    end;
  end;

  if (pt.TokenType in ParamTypes) and pt.HasParentNode(nPropertyParameterList) and
     pt.IsOnRightOf(nPropertyParameterList, ttOpenSquareBracket) then
  begin
    Result := True;
    exit;
  end;

  { single space before class heritage ?
    see NoSpaceAfter }
  if (pt.HasParentNode(nRestrictedType)) and (pt.TokenType in ObjectTypeWords) and
    (FormatSettings.Spaces.SpaceBeforeClassHeritage) then
  begin
    if (ptNext.TokenType in [ttOpenBracket, ttSemiColon]) then
    begin
      Result := True;
      exit;
    end;
  end;

  // else if
  if (pt.TokenType = ttElse) and (ptNext.TokenType = ttIf) and InStatements(pt) then
  begin
    Result := True;
    exit;
  end;

end;


constructor TSingleSpaceAfter.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace, eRemoveReturn];
end;

procedure TSingleSpaceAfter.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext, lcNew: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  { exclude if a comment is next }
  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if lcNext = nil then
    exit;

  if lcNext.TokenType = ttComment then
    exit;

  if NeedsSingleSpace(lcSourceToken, lcNext) then
  begin
    { inspect the next token }
    lcNext := lcSourceToken.NextToken;
    if lcNext.TokenType = ttWhiteSpace then
    begin
      lcNext.SourceCode := AnsiSpace;

      { empty any preceeding whitespace }
      repeat
        lcNext := lcNext.NextToken;
        if lcNext.TokenType = ttWhiteSpace then
          lcNext.SourceCode := '';
      until lcNext.TokenType <> ttWhiteSpace;

    end
    else if (lcNext.TokenType <> ttReturn) then
    begin
      // insert a space
      lcNew := TSourceToken.Create;
      lcNew.TokenType := ttWhiteSpace;
      lcNew.SourceCode := AnsiSpace;

      prVisitResult.Action := aInsertAfter;
      prVisitResult.NewItem := lcNew;
    end;

  end;
end;

end.