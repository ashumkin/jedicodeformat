unit AllProcesses;

{ all warnings put together }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AllProcesses, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.
------------------------------------------------------------------------------*)
{*)}

interface

uses
  { delphi }
  ParseTreeNode, BaseVisitor, ConvertTypes;

type

  TAllProcesses = class(TObject)
  private
    fcOnMessages: TStatusMessageProc;
    fcRoot: TParseTreeNode;

    procedure ApplyVisitorType(const pcVisitorType: TTreeNodeVisitorType);

    procedure Obfuscate;

    procedure ClarifySetup;
    procedure Transform;
    procedure Warnings;
    procedure Spacing;
    procedure LineBreaking;
    procedure Capitalisation;
    procedure Indent;
    procedure Align;

    procedure OnceOffs;
  public
    constructor Create;

    procedure Execute(const pcRoot: TParseTreeNode);

    property OnMessage: TStatusMessageProc Read fcOnMessages Write fcOnMessages;
  end;

implementation

uses
  { delphi }
  Forms,
  { local }
  JcfSettings, SetClarify, VisitSetXY,
  { once-offs }
  //MozComment,
  { obfuscate}
  FixCase, RemoveComment, RemoveBlankLine, RemoveReturn, ReduceWhiteSpace,
  RemoveConsecutiveWhiteSpace, RemoveUnneededWhiteSpace, RebreakLines,
  { transform }
  FindReplace, UsesClauseInsert, UsesClauseRemove, UsesClauseFindReplace,
  RemoveEmptyComment,
  { warnings }
  Warning, WarnEmptyBlock, WarnRealType, WarnAssignToFunctionName,
  WarnCaseNoElse, WarnDestroy,
  { caps}
  UnitNameCaps, SpecificWordCaps, Capitalisation,
  { returns }
  ReturnChars,
  RemoveReturnsAfterBegin, RemoveReturnsBeforeEnd,
  PropertyOnOneLine,
  RemoveBlankLinesAfterProcHeader, RemoveBlankLinesInVars,
  NoReturnBefore, NoReturnAfter, ReturnBefore, ReturnAfter,
  BlockStyles, ReturnsAfterFinalEnd, RemoveConsecutiveReturns,
  { spacing}
  TabToSpace, SpaceToTab, MaxSpaces,
  NoSpaceAfter, NoSpaceBefore, SingleSpaceBefore, SingleSpaceAfter,
  SpaceBeforeColon, RemoveSpaceAtLineEnd, VisitStripEmptySpace,
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
        OnMessage('', lsMessage, -1, -1);

      Application.ProcessMessages;
    end;
  finally
    lc.Free;
  end;
end;

procedure TAllProcesses.Execute(const pcRoot: TParseTreeNode);
begin
  Assert(pcRoot <> nil);
  fcRoot := pcRoot;

  if FormatSettings.Clarify.OnceOffs = eRunOnly then
  begin
    // run only the once-offs
    OnceOffs;
  end
  else if FormatSettings.Obfuscate.Enabled then
  begin
    Obfuscate;
  end
  else
  begin
    // normal clarify path 
    ClarifySetup;
    Transform;
    Warnings;
    Capitalisation;
    LineBreaking;
    Spacing;
    Indent;
    Align;

    // run the once-offs too?
    if FormatSettings.Clarify.OnceOffs = eDoRun then
      OnceOffs;

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
  ApplyVisitorType(TVisitStripEmptySpace);
  ApplyVisitorType(TRebreakLines);
end;

procedure TAllProcesses.ClarifySetup;
begin
  ApplyVisitorType(TVisitSetNestings);
  ApplyVisitorType(TVisitSetXY);
end;

procedure TAllProcesses.Transform;
begin
  ApplyVisitorType(TFindReplace);
  ApplyVisitorType(TUsesClauseInsert);
  ApplyVisitorType(TUsesClauseRemove);
  ApplyVisitorType(TUsesClauseFindReplace);

  ApplyVisitorType(TRemoveEmptyComment);
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
  ApplyVisitorType(TUnitNameCaps);
  ApplyVisitorType(TSpecificWordCaps);
  ApplyVisitorType(TCapitalisation);
end;

procedure TAllProcesses.Spacing;
begin
  ApplyVisitorType(TTabToSpace);
  ApplyVisitorType(TSpaceToTab);
  ApplyVisitorType(TMaxSpaces);

  ApplyVisitorType(TNoSpaceAfter);
  ApplyVisitorType(TNoSpaceBefore);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSingleSpaceBefore);
  ApplyVisitorType(TSingleSpaceAfter);
  ApplyVisitorType(TVisitStripEmptySpace);

  ApplyVisitorType(TSpaceBeforeColon);
  ApplyVisitorType(TRemoveSpaceAtLineEnd);
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
  ApplyVisitorType(TRemoveConsecutiveReturns);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TReturnBefore);

  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TReturnAfter);

  ApplyVisitorType(TBlockStyles);

  { long line breaking is a bit of a circular thing
    The indentation before the code on the line
    influences if (and where) to insert a break

    But indeting needs to happen the linebreaker to indent the trailing line
    Fortunately this is only an issue when code is really badly formatted
    e.g. de-obfucation }

  ApplyVisitorType(TVisitStripEmptySpace);
  ApplyVisitorType(TVisitSetXY);
  ApplyVisitorType(TLongLineBreaker);

  ApplyVisitorType(TReturnsAfterFinalEnd);
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

procedure TAllProcesses.OnceOffs;
begin
  //ApplyVisitorType(TMozComment);
end;

end.
