unit TokenUtils;

{ AFS 2 Jan
  procedureal code that works on the parse tree
  not put on the class as it's most fairly specific stuff
    (but has been put here because 2 or more processes use it )
  and nedds to know both classes - TParseTreeNode and TSoruceTOken
  }

interface

uses ParseTreeNode;

{ return the name of the procedure around any parse tree node or source token
  empty string if there is none }
function GetProcedureName(const pcNode: TParseTreeNode;
  const pbFullName: boolean; const pbTopmost: boolean): string;


{ depending on context, one of Procedure, function, constructor, destructor }
function GetBlockType(const pcNode: TParseTreeNode): string;


function ExtractNameFromFunctionHeading(const pcNode: TParseTreeNode;
  const pbFullName: boolean): string;

implementation

uses ParseTreeNodeType, SourceToken, TokenType;

{ given a function header parse tree node, extract the fn name underneath it }
function ExtractNameFromFunctionHeading(const pcNode: TParseTreeNode;
  const pbFullName: boolean): string;
var
  liLoop: integer;
  lcChildNode: TParseTreeNode;
  lcSourceToken: TSourceToken;
  lcNameToken: TSourceToken;
  lcPriorToken1, lcPriorToken2: TSourceToken;
begin
  lcNameToken := nil;

  { function heading is of one of these forms
      function foo(param: integer): integer;
      function foo: integer;
      function TBar.foo(param: integer): integer;
      function TBar.foo: integer;

    within the fn heading, the name will be last identifier before nFormalParams or ':'

  }
  for liLoop := 0 to pcNode.ChildNodeCount - 1 do
  begin
    lcChildNode := pcNode.ChildNodes[liLoop];

    if lcChildNode.NodeType = nFormalParams then
      break;

    if lcChildNode is TSourceToken then
    begin
      lcSourceToken := TSourceToken(lcChildNode);

      { keep the name of the last identifier }
      if lcSourceToken.TokenType in IdentifierTypes then
        lcNameToken := lcSourceToken
      else if lcSourceToken.TokenType = ttColon then
        break;
    end;
  end;

  if lcNameToken = nil then
    Result := ''
  else if pbFullName then
  begin
    Result := lcNameToken.SourceCode;

    // is it a qualified name
    lcPriorToken1 := lcNameToken.PriorSolidToken;
    if (lcPriorToken1 <> nil) and (lcPriorToken1.TokenType = ttDot) then
    begin
      lcPriorToken2 := lcPriorToken1.PriorSolidToken;
      if (lcPriorToken2 <> nil) and (lcPriorToken2.TokenType in IdentifierTypes) then
      begin
        Result := lcPriorToken2.SourceCode + lcPriorToken1.SourceCode + lcNameToken.SourceCode;
      end;
    end;
  end
  else
  begin
    // just the proc name, no prefix
    Result := lcNameToken.SourceCode;
  end;
end;

const
  PROCEDURE_NODE_TYPES: TParseTreeNodeTypeSet =
    [nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl];


function GetProcedureName(const pcNode: TParseTreeNode;
  const pbFullName: boolean; const pbTopmost: boolean): string;
var
  lcFunction, lcTemp, lcHeading: TParseTreeNode;
begin
  Assert(pcNode <> nil);

  lcFunction := pcNode.GetParentNode(PROCEDURE_NODE_TYPES);

  if lcFunction = nil then
  begin
    // not in a function, procedure or method
    Result := '';
    exit;
  end;

  if pbTopmost then
  begin
    { find the top level function }
    lcTemp := lcFunction.GetParentNode(PROCEDURE_NODE_TYPES);
    while lcTemp <> nil do
    begin
      lcFunction := lcTemp;
      lcTemp := lcFunction.GetParentNode(PROCEDURE_NODE_TYPES);
    end;
  end;

  lcHeading := lCFunction.GetImmediateChild([nFunctionHeading, nProcedureHeading]);

  Result := ExtractNameFromFunctionHeading(lcHeading, pbFullName)
end;

function GetBlockType(const pcNode: TParseTreeNode): string;
var
  lcFunction: TParseTreeNode;
begin
  lcFunction := pcNode.GetParentNode(PROCEDURE_NODE_TYPES + [nInitSection]);

  if lcFunction = nil then
  begin
    Result := '';
    exit;
  end;

  case lcFunction.NodeType of
    nProcedureDecl:
      Result := 'procedure';
    nFunctionDecl:
      Result := 'function';
    nConstructorDecl:
      Result := 'constructor';
    nDestructorDecl:
      Result := 'destructor';
    nInitSection:
      Result := 'initialization section';
    else
      Result := '';
  end;
end;

end.
