unit ReturnBefore;

{ AFS 10 Jan 2003
  Return before
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReturnBefore, released May 2003.
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

uses SwitchableVisitor, VisitParseTree;


type
  TReturnBefore = class(TSwitchableVisitor)
  private
    fiReturnsBefore, fiNextReturnsBefore: integer;
  protected
    procedure InspectSourceToken(const pcToken: TObject); override;

    procedure EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  JclStrings,
  JcfMiscFunctions, TokenUtils,
  SourceToken, Tokens, ParseTreeNode,
  Nesting, ParseTreeNodeType, JcfSettings,
  FormatFlags;

const
  WordsReturnBefore: TTokenTypeSet =
    [ttBegin, ttEnd, ttUntil, ttElse, ttTry, ttFinally, ttExcept];

  WordsBlankLineBefore: TTokenTypeSet =
    [ttImplementation, ttInitialization, ttFinalization, ttUses];


function NeedsBlankLine(const pt, ptNext: TSourceToken): boolean;
var
  lcNext, lcPrev: TSourceToken;
  lcParent: TParseTreeNode;
begin
  Result := (pt.TokenType in WordsBlankLineBefore);
  if Result then
    exit;

  { function/proc body needs a blank line
   but not in RHSEquals of type defs,
   but not in class & interface def,
   but not if precedeed by the class specified for class functions
   but not if it doesn't have a proc body

   IMHO should also have blank line before contained procs
   }

  if (pt.TokenType in ProcedureWords) and
    (not pt.IsOnRightOf(nTypeDecl, ttEquals)) and
    (not IsClassFunction(pt)) and
    (ProcedureHasBody(pt)) then
  begin
    Result := True;
    exit;
  end;

  // form dfm comment
  if IsDfmIncludeDirective(pt) or IsGenericResIncludeDirective(pt) then
  begin
    Result := True;
    exit;
  end;

    { blank line before the words var, type or const at top level
      except for:
      type t2 = type integer; }
  if (pt.TokenType in Declarations) and (pt.Nestings.Total = 0) and
    (not pt.IsOnRightOf(nTypeDecl, ttEquals)) then
  begin
    Result := True;
    exit;
  end;

  { start of class function body }
  if (pt.TokenType = ttClass) and
    (not pt.HasParentNode([nVarDecl, nConstDecl, nClassDeclarations])) and
    (pt.HasParentNode(nFunctionHeading, 1)) then
  begin
    Result := True;
    exit;
  end;

  { interface, but not as a typedef }
  if (pt.TokenType = ttInterface) and not (pt.HasParentNode(nTypeDecl)) then
  begin
    Result := True;
    exit;
  end;


  {
    before class/interface def with body when it's not the first type.

    e.g.
      type
        foo = integer;

        TSomeClass = class...

    These start with a type name
   and have a parent node nTypeDecl, which in turn owns a Restircted type -> Class type
  }
  if IsIdentifier(pt) and pt.HasParentNode(nTypeDecl, 2) then
  begin
    lcPrev := pt.PriorSolidToken;
    if (lcPrev <> nil) and (lcPrev.TokenType <> ttType) then
    begin
      // identifier
      lcParent := pt.Parent;
      if lcParent.NodeType  = nIdentifier then
        lcParent := lcParent.Parent
      else
        lcParent := nil;

      if (lcParent <> nil) then
      begin
        if (lcParent.NodeType = nTypeDecl) and
          lcParent.HasChildNode(ObjectTypes, 2) and
          lcParent.HasChildNode(ObjectBodies, 3) then
        begin
          Result := True;
          exit;
        end;

        { likewise before a record type }
        if (lcParent.NodeType = nTypeDecl) and
          lcParent.HasChildNode(nRecordType, 2) and
          lcParent.HasChildNode(nFieldDeclaration, 3) then
        begin
          Result := True;
          exit;
        end;
      end;
    end;
  end;

  { end. where there is no initialization section code,
    ie 'end' is the first and only token in the init section   }
  if (pt.TokenType = ttEnd) and
    pt.HasParentNode(nInitSection, 1) and
    (pt.Parent.SolidChildCount = 1) then
  begin
    lcNext := pt.NextSolidToken;
    if (lcNext <> nil) and (lcNext.TokenTYpe = ttDot) then
    begin
      Result := True;
      exit;
    end;
  end;
end;


function NeedsReturn(const pt, ptNext: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  Result := (pt.TokenType in WordsReturnBefore);
  if Result = True then
    exit;

  { there is not always a return before 'type'
    e.g.
    type TMyInteger = type Integer;
    is legal, only a return before the first one

   var, const, type but not in parameter list }
  if (pt.TokenType in Declarations) and pt.HasParentNode(nTopLevelSections, 1)
    and (not pt.IsOnRightOf(nTypeDecl, ttEquals)) then
  begin
    Result := True;
    exit;
  end;

  { procedure & function in class def get return but not blank line before }
  if (pt.TokenType in ProcedureWords + [ttProperty]) and
    (pt.HasParentNode([nClassType, nClassType])) and
    (not IsClassFunction(pt)) then
  begin
    Result := True;
    exit;
  end;

  { nested procs get it as well }
  if (pt.TokenType in ProcedureWords) and (not pt.HasParentNode(nProcedureDecl)) and
    (not IsClassFunction(pt)) and
    (not pt.HasParentNode(nType)) then
  begin
    Result := True;
    exit;
  end;

  { start of class function decl in class }
  if (pt.TokenType = ttClass) and pt.HasParentNode([nProcedureDecl, nFunctionDecl]) and
    pt.HasParentNode(nClassDeclarations) and
    (not pt.HasParentNode([nVarDecl, nConstDecl])) then
  begin
    Result := True;
    exit;
  end;

  { access specifiying directive (private, public et al) in a class def }
  if pt.HasParentNode(nClassType) and IsClassDirective(pt) then
  begin
    Result := True;
    exit;
  end;

  { return before 'class' in class function }
  if (pt.TokenType = ttClass) and pt.HasParentNode(ProcedureHeadings) and
    (RoundBracketLevel(pt) < 1) then
  begin
    Result := True;
    exit;
  end;

  { "uses UnitName in 'File'" has a blank line before UnitName }
  if IsIdentifier(pt) and (pt.HasParentNode(nUses)) and (ptNext.TokenType = ttIn) then
  begin
    Result := True;
    exit;
  end;

  // guid in interface
  if (pt.TokenType = ttOpenSquareBracket) and pt.HasParentNode(nInterfaceTypeGuid, 1) then
  begin
    Result := True;
    exit;
  end;

end;

constructor TReturnBefore.Create;
begin
  inherited;
  fiReturnsBefore := 0;
  fiNextReturnsBefore := 0;
  FormatFlags := FormatFlags + [eAddReturn];
end;

procedure TReturnBefore.EnabledVisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  liReturnsNeeded: integer;
begin
  lcSourceToken := TSourceToken(pcToken);
  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  liReturnsNeeded := 0;

  if NeedsBlankLine(lcSourceToken, lcNext) then
    liReturnsNeeded := 2
  else if NeedsReturn(lcSourceToken, lcNext) then
    liReturnsNeeded := 1;


  { number to insert = needed - actual }
  liReturnsNeeded := liReturnsNeeded - fiReturnsBefore;

  if liReturnsNeeded > 0 then
  begin

    case liReturnsNeeded of
      1:
      begin
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewReturn;
      end;
      2:
      begin
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewReturn;
        prVisitResult.NewItem2 := NewReturn;
      end;
      else
      begin
        Assert(False, 'Too many returns');
      end;
    end;
  end;

end;

procedure TReturnBefore.InspectSourceToken(const pcToken: TObject);
var
  lcSourceToken: TSourceToken;
begin
  {
    inspect the tokens as they go past
    this is a running total, that is affeced by returns & non-white-space chars
   A comment line is as good as a blank line for this

    if we encounter the tokens <return> <spaces> <word-needing-return before> the flag must be set true
   }
   fiReturnsBefore := fiNextReturnsBefore;

  lcSourceToken := TSourceToken(pcToken);

  if (lcSourceToken.TokenType = ttReturn) then
    inc(fiNextReturnsBefore)
  else if not (lcSourceToken.TokenType in [ttReturn, ttWhiteSpace, ttComment]) then
    fiNextReturnsBefore := 0;

end;

function TReturnBefore.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Returns.AddGoodReturns;
end;

end.