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

function ExtractNameFromFunctionHeading(const pcNode: TParseTreeNode): string;

implementation

uses ParseTreeNodeType, SourceToken, TokenType;

{ given a function header parse tree node, extract the fn name underneath it }
function ExtractNameFromFunctionHeading(const pcNode: TParseTreeNode): string;
var
  liLoop: integer;
  lcChildNode: TParseTreeNode;
  lcSourceToken: TSourceToken;
begin
  Result := '';

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
        Result := lcSourceToken.SourceCode
      else if lcSourceToken.TokenType = ttColon then
        break;
    end;
  end;
end;

function GetProcedureName(const pcNode: TParseTreeNode;
  const pbFullName: boolean; const pbTopmost: boolean): string;
const
  PROCEDURE_NODE_TYPES: TParseTreeNodeTypeSet =
    [nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl];
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

  Result := ExtractNameFromFunctionHeading(lcHeading)
end;


end.
