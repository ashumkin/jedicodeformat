unit WarnCaseNoElse;

{ AFS 20 June 2K
 warn of case without a default 'else' case

 This is often an error
 your program will be more error-proof if every case has an else
 if you can't think of anything to put there, put

 case
    ...
    else Raise Exception.Create('case had unexpected value');
 end;
}

interface

uses Warning, VisitParseTree;

type

  TWarnCaseNoElse = class(TWarning)
    public
      procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  end;


implementation

uses
  ParseTreeNode, ParseTreeNodeType, WordMap;



procedure TWarnCaseNoElse.PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcNode: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  if lcNode.NodeType <> nCaseStatement then
    exit;

  // we have a case statement
  if not lcNode.HasChildNode([wElse], 1) then
  begin
     SendWarning(lcNode, 'Case statement has no else case');
  end;
end;

end.
