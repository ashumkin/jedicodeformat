unit BaseVisitor;

{ AFS 28 Dec 2002

  Base class that implments the tree node Visitor interface
}

interface

uses VisitParseTree;

type

  TBaseTreeNodeVisitor = class(TInterfacedObject, IVisitParseTree)

  public
    constructor Create; virtual;

    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); virtual;
    procedure PostVisitParseTreeNode(const pcNode: TObject); virtual;
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); virtual;

    function FinalSummary(var psMessage: string): Boolean; virtual;
  end;

type
  TTreeNodeVisitorType = class of TBaseTreeNodeVisitor;

implementation


// need a virtual constructor for the create-by-class-ref
constructor TBaseTreeNodeVisitor.Create;
begin
  inherited;
end;

function TBaseTreeNodeVisitor.FinalSummary(var psMessage: string): Boolean;
begin
  // no message
  Result := False;
  psMessage := '';
end;

procedure TBaseTreeNodeVisitor.PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
begin
  // do nothing, here for override
end;

procedure TBaseTreeNodeVisitor.PostVisitParseTreeNode(const pcNode: TObject);
begin
  // do nothing, here for override
end;

procedure TBaseTreeNodeVisitor.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
begin
  // do nothing, here for override
end;

end.
