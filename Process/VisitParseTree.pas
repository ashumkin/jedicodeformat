unit VisitParseTree;

{ AFS 28 December 2002

  Define the interface for the visitor pattern
  to visit tree nodes
}

interface

type

  // action on visiting a node.
  TVisitAction = (aNone, aDelete, aInsertAfter, aInsertBefore);

  // what happens on visiting a node. More fields to come?
  TRVisitResult = record
    Action: TVisitAction;
    NewItem: TObject;
    NewItem2: TObject;
  end;

  IVisitParseTree = interface

    { there are two kinds of node - interior (parse tree node)
      and leaf (source token) }
    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
    procedure PostVisitParseTreeNode(const pcNode: TObject);
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);

  end;

procedure ClearVisitResult(var prVisitResult: TRVisitResult);

implementation

procedure ClearVisitResult(var prVisitResult: TRVisitResult);
begin
  prVisitResult.Action := aNone;
  prVisitResult.NewItem := nil;
  prVisitResult.NewItem2 := nil;
end;

end.
