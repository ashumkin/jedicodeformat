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

    procedure Obfuscate;

    procedure ClarifySetup;
    procedure Warnings;
    procedure Spacing;
    procedure Capitalisation;
    procedure Indent;
  public
    constructor Create;

    procedure Execute(const pcRoot: TParseTreeNode);

    property OnMessage: TStatusMessageProc read fcOnMessages write fcOnMessages;
end;

implementation

uses
  JcfSettings,
  VisitSetXY,

  { obfuscate}
  FixCase, RemoveComment, RemoveBlankLine, RemoveReturn, ReduceWhiteSpace,
  RemoveConsecutiveWhiteSpace, RemoveUnneededWhiteSpace, RebreakLines,
  { warnings }
  Warning, WarnEmptyBlock, WarnRealType, WarnAssignToFunctionName,
  WarnCaseNoElse, WarnDestroy,
  { caps}
  SpecifiCWordCaps, Capitalisation,
  { spacing}
  PropertyOnOneLine,
  NoReturnBefore, NoReturnAfter, ReturnBefore, ReturnAfter,
  NoSpaceAfter, NoSpaceBefore, SingleSpaceBefore, SingleSpaceAfter,
  SpaceBeforeColon, VisitStripEmptySpace, 
  BlockStyles,
  {indent}
  VisitSetNesting, Indenter;

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
    ClarifySetup;
    Warnings;
    Capitalisation;
    Spacing;
    Indent;
  end;
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

procedure TAllProcesses.ClarifySetup;
begin
  ApplyVisitorType(TVisitSetNestings);
  ApplyVisitorType(TVisitSetXY);
end;

procedure TAllProcesses.Warnings;
begin
  ApplyVisitorType(TVisitSetXY);

  ApplyVisitorType(TWarnEmptyBlock);
  ApplyVisitorType(TWarnRealType);
  ApplyVisitorType(TWarnAssignToFunctionName);
  ApplyVisitorType(TWarnCaseNoElse);
  ApplyVisitorType(TWarnDestroy);
end;

procedure TAllProcesses.Capitalisation;
begin
  ApplyVisitorType(TSpecificWordCaps);
  ApplyVisitorType(TCapitalisation);
end;

procedure TAllProcesses.Spacing;
begin
  // apply them all
  ApplyVisitorType(TPropertyOnOneLine);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TNoReturnAfter);
  ApplyVisitorType(TNoReturnBefore);
  ApplyVisitorType(TReturnAfter);

  ApplyVisitorType(TNoSpaceAfter);
  ApplyVisitorType(TNoSpaceBefore);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSingleSpaceBefore);
  ApplyVisitorType(TSingleSpaceAfter);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSpaceBeforeColon);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TBlockStyles);
end;

procedure TAllProcesses.Indent;
begin
  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TIndenter);
  ApplyVisitorType(TVisitSetXY);
end;

end.
