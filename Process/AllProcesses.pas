unit AllProcesses;

{ all warnings put together }

interface

uses
  { delphi } Classes,
  ParseTreeNode, BaseVisitor, ConvertTypes;

type

TAllProcesses = class(TObject)
  private
    fcOnMessages: TStatusMessageProc;
    fcRoot: TParseTreeNode;

    procedure ApplyVisitorType(const pcVisitorType: TTreeNodeVisitorType);

    procedure AllWarnings;
    procedure Obfuscate;
  public
    constructor Create;

    procedure Execute(const pcRoot: TParseTreeNode);

    property OnMessage: TStatusMessageProc read fcOnMessages write fcOnMessages;
end;

implementation

uses
  JcfSettings,
  VisitSetXY,

  { warnings }
  Warning, WarnEmptyBlock, WarnRealType, WarnAssignToFunctionName,
  WarnCaseNoElse, WarnDestroy,
  { obfuscate}
  FixCase, RemoveComment, RemoveBlankLine, RemoveReturn, ReduceWhiteSpace,
  RemoveConsecutiveWhiteSpace, RemoveUnneededWhiteSpace, RebreakLines;

constructor TAllProcesses.Create;
begin
  inherited;
  fcOnMessages := nil;
end;


procedure TAllProcesses.ApplyVisitorType(const pcVisitorType: TTreeNodeVisitorType);
var
  lc: TBaseTreeNodeVisitor;
begin
  Assert(fcRoot <> nil);

  lc := pcVisitorType.Create;

  if (lc is TWarning) then
    (lc as TWarning).OnWarning := OnMessage;

  fcRoot.VisitTree(lc);

end;

procedure TAllProcesses.Execute(const pcRoot: TParseTreeNode);
begin
  Assert(pcRoot <> nil);
  fcRoot := pcRoot;

  if Settings.Obfuscate.Enabled then
  begin
    Obfuscate;
  end
  else
  begin
    AllWarnings;
  end;

end;

procedure TAllProcesses.AllWarnings;
begin
  ApplyVisitorType(TVisitSetXY);

  ApplyVisitorType(TWarnEmptyBlock);
  ApplyVisitorType(TWarnRealType);
  ApplyVisitorType(TWarnAssignToFunctionName);
  ApplyVisitorType(TWarnCaseNoElse);
  ApplyVisitorType(TWarnDestroy);
end;

procedure TAllProcesses.Obfuscate;
begin
  // apply them all
  ApplyVisitorType(TFixCase);
  ApplyVisitorType(TRemoveComment);
  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TRemoveBlankLine);
  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TRemoveReturn);
  ApplyVisitorType(TReduceWhiteSpace);
  ApplyVisitorType(TRemoveConsecutiveWhiteSpace);

  ApplyVisitorType(TRemoveUnneededWhiteSpace);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TRebreakLines);
end;


end.
