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

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  JclStrings,
  SourceToken, Tokens, ParseTreeNodeType, JcfSettings,
  FormatFlags, TokenUtils;

const
  // space before all operators
  SingleSpaceBeforeWords: TTokenTypeSet = [ttEquals, ttThen, ttOf, ttDo,
    ttTo, ttDownTo,
    // some unary operators
    ttNot,
    // all operators that are always binary
    ttAnd, ttAs, ttDiv, ttIn, ttIs, ttMod, ttOr, ttShl, ttShr, ttXor,
    ttTimes, ttFloatDiv, ttEquals, ttGreaterThan, ttLessThan,
    ttGreaterThanOrEqual, ttLessThanOrEqual, ttNotEqual];

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

  if IsHintDirective(pt) then
  begin
    Result := True;
    exit;
  end;

  { 'a := --3;' and 'lc := ptr^;'
  are the only exceptions to the rule of a space before an operator }
  if (pt.TokenType in Operators) then
  begin
    if (pt.TokenType = ttHat) or 
      (IsUnaryOperator(pt) and IsUnaryOperator(pt.PriorSolidToken)) then
      Result := False
    else
      Result := True;
      
    exit;
  end;

  if (pt.TokenType in AllDirectives) and (pt.HasParentNode(DirectiveNodes)) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType in SingleSpaceBeforeWords) then
  begin
    Result := True;
    exit;
  end;

  { 'in' in the uses clause }
  if ((pt.TokenType = ttIn) and (pt.HasParentNode(nUses))) then
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

  { string that starts with # , ie char codes
  }
  if HashLiteral(pt) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType = ttDefault) and pt.HasParentNode(nPropertySpecifier) then
  begin
    Result := True;
    exit;
  end;

  { signle space before read, write etc in property }
  if pt.HasParentNode(nProperty) then
  begin
    if (pt.TokenType in [ttProperty, ttRead, ttWrite, ttDefault, ttStored, ttNoDefault, ttImplements]) then
    begin
      Result := True;
      exit;
    end;
  end;


  { program uses form link comment }
  if InFilesUses(pt) then
  begin
    if ((pt.TokenType = ttComment) and (pt.CommentStyle in CURLY_COMMENTS)) and
      pt.IsOnRightOf(nUses, ttUses) then
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

function TSingleSpaceBefore.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.FixSpacing;
end;

end.