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
    procedure LineBreaking;
    procedure Capitalisation;
    procedure Indent;
    procedure Align;
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
  { returns }
  ReturnChars,
  RemoveReturnsAfterBegin, RemoveReturnsBeforeEnd,
  PropertyOnOneLine,
  RemoveBlankLinesAfterProcHeader, RemoveBlankLinesInVars,
  NoReturnBefore, NoReturnAfter, ReturnBefore, ReturnAfter,
  BlockStyles,
  { spacing}
  NoSpaceAfter, NoSpaceBefore, SingleSpaceBefore, SingleSpaceAfter,
  SpaceBeforeColon, VisitStripEmptySpace,
  {indent}
  VisitSetNesting, Indenter, LongLineBreaker,
  { stats }
  BasicStats,
  { align }
  AlignConst, AlignVars, AlignAssign, AlignTypedef, AlignComment;

constructor TAllProcesses.Create;
begin
  inherited;
  fcOnMessages := nil;
end;


procedure TAllProcesses.ApplyVisitorType(const pcVisitorType: TTreeNodeVisitorType);
var
  lc: TBaseTreeNodeVisitor;
  lsMessage: string;
begin
  Assert(fcRoot <> nil);

  lc := pcVisitorType.Create;
  try
    if lc.IsIncludedInSettings then
    begin

      if (lc is TWarning) then
        (lc as TWarning).OnWarning := OnMessage;

      fcRoot.VisitTree(lc);

      if lc.FinalSummary(lsMessage) then
        OnMessage(lsMessage);
    end;
  finally
    lc.Free;
  end;
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
    LineBreaking;
    Spacing;
    Indent;
    Align;

    // stats last 
    ApplyVisitorType(TBasicStats);
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
  ApplyVisitorType(TNoSpaceAfter);
  ApplyVisitorType(TNoSpaceBefore);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSingleSpaceBefore);
  ApplyVisitorType(TSingleSpaceAfter);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSpaceBeforeColon);
  ApplyVisitorType(TVisitStripEmptySpace);
end;


procedure TAllProcesses.LineBreaking;
begin
  ApplyVisitorType(TReturnChars);

  ApplyVisitorType(TPropertyOnOneLine);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TRemoveBlankLinesAfterProcHeader);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TRemoveBlankLinesInVars);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TRemoveReturnsAfterBegin);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TRemoveReturnsBeforeEnd);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TNoReturnAfter);
  ApplyVisitorType(TNoReturnBefore);
  ApplyVisitorType(TReturnAfter);

  ApplyVisitorType(TBlockStyles);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TLongLineBreaker);
end;


procedure TAllProcesses.Indent;
begin
  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TIndenter);
  ApplyVisitorType(TVisitSetXY);
end;

procedure TAllProcesses.Align;
begin

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TAlignConst);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TAlignVars);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TAlignAssign);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TAlignTypedef);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TAlignComment);
end;

end.
