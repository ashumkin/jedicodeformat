unit WarnAssignToFunctionName;

{ AFS 21 Sept 2001

 warn of assignment to function name in old TurboPascal code

 ie
  function Fred: integer;
  begin
    Fred := 3;
  end;

 should be

  function Fred: integer;
  begin
    Result := 3;
  end;
}

interface

uses Warning, VisitParseTree;

type

  TWarnAssignToFunctionName = class(TWarning)
    private
      procedure WarnAllAssigns(const psFnName: string; const pcRoot: TObject);
    public
      procedure VisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  { delphi } SysUtils,
  ParseTreeNode, ParseTreeNodeType, SourceToken, TokenType;


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

{ get the node that represents the identifier that is being assigned to
  node passed in will be statement

  looking for the last id before the ':=',

  e.g. in "TFoo(bar.baz) := fish;" we want "baz"

  NB this may not work in complex examples as the id may be under an expr node
  but may suffice for this fn name assign detection
  }
function GetIdentifierBeforeAssign(const pcNode: TParseTreeNode): TSourceToken;
var
  liLoop: integer;
  lcChildNode: TParseTreeNode;
  lcSourceToken: TSourceToken;
begin
  Result := nil;

  for liLoop := 0 to pcNode.ChildNodeCount - 1 do
  begin
    lcChildNode := pcNode.ChildNodes[liLoop];

    if lcChildNode is TSourceToken then
    begin
      lcSourceToken := TSourceToken(lcChildNode);

      if lcSourceToken.TokenType in IdentifierTypes then
        Result := lcSourceToken
      else if lcSourceToken.TokenType = ttAssign then
        break;
    end;

  end;
end;

procedure TWarnAssignToFunctionName.VisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcNode: TParseTreeNode;
  lcFunctionHeading: TParseTreeNode;
  lsName: string;
begin
  lcNode := TParseTreeNode(pcNode);

  if lcNode.NodeType <> nFunctionDecl then
    exit;

  { we now have a function decl
    Find the name, find the assign statements. Compare }
  lcFunctionHeading := lcNode.GetImmediateChild([nFunctionHeading]);
  Assert(lcFunctionHeading <> nil);

  lsName := ExtractNameFromFunctionHeading(lcFunctionHeading);

  WarnAllAssigns(lsName, lcNode);
end;

procedure TWarnAssignToFunctionName.WarnAllAssigns(const psFnName: string;
  const pcRoot: TObject);
var
  lcNode: TParseTreeNode;
  lcLeftName: TSOurceToken;
  liLoop: integer;
begin
  Assert(pcRoot <> nil);
  lcNode := TParseTreeNode(pcRoot);

  if (lcNode.NodeType = nStatement) and (lcNode.HasChildNode(ttAssign, 1)) then
  begin

    // this is an assign statement. Look at the LHS
    lcLeftName := GetIdentifierBeforeAssign(lcNode);

    if AnsiSameText(lcLeftName.SourceCode, psFnName) then
    begin
      SendWarning(lcLeftName,
        'Assignment to the function name "' + psFnName +
          '" is deprecated, Use assignment to "Result"');
    end;
  end
  else
  begin
    // look at all nodes under here
    for liLoop := 0 to lcNode.ChildNodeCount - 1 do
      WarnAllAssigns(psFnName, lcNode.ChildNodes[liLoop]);
  end;
end;

end.
