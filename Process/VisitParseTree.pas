unit VisitParseTree;

{ AFS 28 December 2002

  Define the interface for the visitor pattern
  to visit tree nodes
}

interface

type

  // action on visiting a node. More to come
  TVisitAction = (aNone, aDelete, aInsertAfter);

  // what happens on visiting a node. More fields to come?
  TRVisitResult = record
    Action: TVisitAction;
    NewItem: TObject;
  end;

  IVisitParseTree = interface

    { there are two kinds of node - itnerior (parse tree node)
      and leaf (source token) }
    procedure VisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);

  end;

procedure ClearVisitResult(var prVisitResult: TRVisitResult);

implementation

procedure ClearVisitResult(var prVisitResult: TRVisitResult);
begin
  prVisitResult.Action := aNone;
  prVisitResult.NewItem := nil;
end;

end.
