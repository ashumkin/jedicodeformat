unit SingleSpaceBefore;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SingleSpaceBefore, released May 2003.
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


{ AFS 7 Dec 1999
  single space before certain tokens (e.g. ':='

  This process and SingleSpaceAfter must be carefull with directives:
   words like "read" and "write" must be single-spaced in property defs
   but in normal code these are valid procedure names, and
     converting "Result := myObject.Read;" to
     "Result := myObject. read ;" compiles, but looks all wrong
}

uses SwitchableVisitor, VisitParseTree;


type
  TSingleSpaceBefore = class(TSwitchableVisitor)
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
  SourceToken, TokenType, WordMap, ParseTreeNodeType, JcfSettings,
  FormatFlags, TokenUtils;

const
  // space before all operators
  SingleSpaceBeforeWords: TWordSet = [wEquals, wThen, wOf, wDo,
    wTo, wDownTo,
    // some unary operators
    wNot,
    // all operators that are always binary
    wAnd, wAs, wDiv, wIn, wIs, wMod, wOr, wShl, wShr, wXor,
    wTimes, wFloatDiv, wEquals, wGreaterThan, wLessThan,
    wGreaterThanOrEqual, wLessThanOrEqual, wNotEqual];

function NeedsSpaceBefore(const pt: TSourceToken): boolean;
begin
  Result := False;

  { not in ASM block }
  if pt.HasParentNode(nAsm) then
    exit;

  if (pt.TokenType = ttAssign) then
  begin
    Result := True;
    exit;
  end;

  { 'a := --3;' and 'lc := ptr^;'
  are the only exceptions to the rule of a space before an operator }
  if (pt.TokenType = ttOperator) then
  begin
    if (pt.Word = wHat) or 
      (IsUnaryOperator(pt) and IsUnaryOperator(pt.PriorSolidToken)) then
      Result := False
    else
      Result := True;
      
    exit;
  end;

  if (pt.Word in AllDirectives) and (pt.HasParentNode(DirectiveNodes)) then
  begin
    Result := True;
    exit;
  end;

  if (pt.Word in SingleSpaceBeforeWords) then
  begin
    Result := True;
    exit;
  end;

  { 'in' in the uses clause }
  if ((pt.Word = wIn) and (pt.HasParentNode(nUses))) then
  begin
    Result := True;
    exit;
  end;

  { 'absolute' as a var directive }
  if (pt.Word = wAbsolute) and pt.HasParentNode(nAbsoluteVar) then
  begin
    Result := True;
    exit;
  end;

  { string that starts with # , ie char codes
  }
  if pt.IsHashLiteral then
  begin
    Result := True;
    exit;
  end;

  if (pt.Word = wDefault) and pt.HasParentNode(nPropertySpecifier) then
  begin
    Result := True;
    exit;
  end;

  { program uses form link comment }
  if InFilesUses(pt) then
  begin
    if ((pt.TokenType = ttComment) and (pt.CommentStyle = eCurly)) and
      pt.IsOnRightOf(nUses, wUses) then
    begin
      Result := True;
      exit;
    end;
  end;


end;


constructor TSingleSpaceBefore.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace, eRemoveReturn];
end;

procedure TSingleSpaceBefore.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcNext, lcNew: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);
  lcNext := lcSourceToken.NextToken;

  if lcNext = nil then
    exit;

  if NeedsSpaceBefore(lcNext) then
  begin
    if (lcSourceToken.TokenType = ttWhiteSpace) then
    begin
      { one space }
      lcSourceToken.SourceCode := AnsiSpace;

      { empty any preceeding whitespace }
      repeat
        lcSourceToken := lcSourceToken.PriorToken;
        if lcSourceToken.TokenType = ttWhiteSpace then
          lcSourceToken.SourceCode := '';
      until lcSourceToken.TokenType <> ttWhiteSpace;
    end
    else
    begin
      lcNew := TSourceToken.Create;
      lcNew.TokenType := ttWhiteSpace;
      lcNew.SourceCode := AnsiSpace;

      prVisitResult.Action := aInsertAfter;
      prVisitResult.NewItem := lcNew;
    end;
  end;


end;

end.