unit ObfuscateControl;

{ this unit fires off the obfuscation procedures }

interface

uses ParseTreeNode, BaseVisitor, VisitParseTree;

procedure Obfuscate(const pcRoot: TParseTreeNode);

implementation

uses FixCase, RemoveComment, RemoveReturn, ReduceWhiteSpace,
  RemoveConsecutiveWhiteSpace, RemoveUnneededWhiteSpace, RebreakLines;

procedure ApplyVisitorType(const pcRoot: TParseTreeNode; const pcVisitorType: TTreeNodeVisitorType);
var
  lc: TBaseTreeNodeVisitor;
begin
  lc := pcVisitorType.Create;

  pcRoot.VisitTree(lc);
end;

procedure Obfuscate(const pcRoot: TParseTreeNode);
begin
  // apply them all 
  ApplyVisitorType(pcRoot, TFixCase);
  ApplyVisitorType(pcRoot, TRemoveComment);
  ApplyVisitorType(pcRoot, TRemoveReturn);
  ApplyVisitorType(pcRoot, TReduceWhiteSpace);
  ApplyVisitorType(pcRoot, TRemoveConsecutiveWhiteSpace);

  ApplyVisitorType(pcRoot, TRemoveUnneededWhiteSpace);

  ApplyVisitorType(pcRoot, TRebreakLines);

end;

end.
