unit WarnEmptyBlock;

{ AFS 30 Dec 2002
 warn of an enmpty block, one of
 begin..end, try..except, try..finally, except..end, finally..end
}


interface


uses Warning, VisitParseTree;

type

  TWarnEmptyBlock = class(TWarning)
    public
      procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;

implementation

uses ParseTreeNode, ParseTreeNodeType;

procedure TWarnEmptyBlock.PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcNode: TParseTreeNode;
  liSolidChildCount: integer;
begin
  lcNode := TParseTreeNode(pcNode);

  // only look in statements
  if not lcNode.HasParentNode(nBlock) then
    exit;

  { looking for nodes with 2 solid tokens under them
    e.g. 'begin' and 'end'
  }
  liSolidChildCount := lcNode.SolidChildCount;
  
  if liSolidChildCount = 2 then
  begin
    if lcNode.NodeType = nCompoundStatement then
    begin
      SendWarning(lcNode, 'Empty begin..end block');
    end;

    if lcNode.NodeType = nFinallyBlock then
    begin
      SendWarning(lcNode, 'Empty finally..end block');
    end;

    if lcNode.NodeType = nExceptBlock then
    begin
      SendWarning(lcNode, 'Empty except..end block');
    end;
  end
  else if liSolidChildCount = 1 then
  begin
    if lcNode.NodeType = nTryBlock then
    begin
      SendWarning(lcNode, 'Empty try block');
    end;
  end;
  
end;

end.
