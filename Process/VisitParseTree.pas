unit VisitParseTree;

{ AFS 28 December 2002

  Define the interface for the visitor pattern
  to visit tree nodes
}

interface

type

  IVisitParseTree = interface

    { there are two kinds of node - itnerior (parse tree node)
      and leaf (source token) }
    procedure VisitParseTreeNode(const pcNode: TObject);
    procedure VisitSourceToken(const pcToken: TObject);

  end;

implementation

end.
