unit AllWarnings;

{ all warnings put together }

interface

uses
  { delphi } Classes,
  ParseTreeNode, BaseVisitor, ConvertTypes;

type

TAllWarnings = class(TObject)
  private
    fcOnWarnings: TStatusMessageProc;

    procedure ApplyVisitorType(const pcRoot: TParseTreeNode; const pcVisitorType: TTreeNodeVisitorType);

  public
    constructor Create; 

    procedure Execute(const pcRoot: TParseTreeNode);

    property OnWarning: TStatusMessageProc read fcOnWarnings write fcOnWarnings;
end;

implementation

uses Warning, VisitSetXY,
  WarnEmptyBlock, WarnRealType, WarnAssignToFunctionName,
  WarnCaseNoElse, WarnDestroy;

constructor TAllWarnings.Create;
begin
  inherited;
  fcOnWarnings := nil;
end;


procedure TAllWarnings.ApplyVisitorType(const pcRoot: TParseTreeNode;
  const pcVisitorType: TTreeNodeVisitorType);
var
  lc: TBaseTreeNodeVisitor;
begin
  lc := pcVisitorType.Create;

  if (lc is TWarning) then
    (lc as TWarning).OnWarning := OnWarning;

  pcRoot.VisitTree(lc);

end;

procedure TAllWarnings.Execute(const pcRoot: TParseTreeNode);
begin
  ApplyVisitorType(pcRoot, TVisitSetXY);

  ApplyVisitorType(pcRoot, TWarnEmptyBlock);
  ApplyVisitorType(pcRoot, TWarnRealType);
  ApplyVisitorType(pcRoot, TWarnAssignToFunctionName);
  ApplyVisitorType(pcRoot, TWarnCaseNoElse);
  ApplyVisitorType(pcRoot, TWarnDestroy);
end;

end.
