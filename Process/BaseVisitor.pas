unit BaseVisitor;

{ AFS 28 Dec 2002

  Base class that implments the tree node Visitor interface
}

interface

uses VisitParseTree;

type

  TBaseTreeNodeVisitor = class(TInterfacedObject, IVisitParseTree)
  public
    procedure VisitParseTreeNode(const pcNode: TObject); virtual;
    procedure VisitSourceToken(const pcToken: TObject); virtual;

  end;

implementation

{ TBaseTreeNodeVisitor }

procedure TBaseTreeNodeVisitor.VisitParseTreeNode(const pcNode: TObject);
begin
  // do nothing
end;

procedure TBaseTreeNodeVisitor.VisitSourceToken(const pcToken: TObject);
begin
  // do nothing
end;

end.
