unit TreeWalker;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SimpleTreeWalker, released March 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2004 Anthony Steele.
All Rights Reserved.
Contributor(s):
Anthony Steele.

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

{ AFS 1 March 04

  Simpler, hopefully faster approach to visiting the tree
  Code is in neither the visitor or the tree
  but uses both

  The tree is a root node with children
  The visitor is a process which is applied to each node in turn

  This is how all processes that transform the program input to output
  are applied. So it is key to the second phase of the program,
  The first being generating the parse tree
}

uses ParseTreeNode, BaseVisitor, VisitParseTree;

type
  TTreeWalker = class(TObject)
  private
    { flags about the current visitor's needs
      Almost all visitors use the infix walk of leaf nodes
      Few look at the interior nodes }

    { does it do the prefix walk of interior nodes? }
    fbHasPreVisit: Boolean;
    { does it do the postfix walk of interior nodes? }
    fbHasPostVisit: Boolean;

    { does it visit the leaves - almost all do }
    fbHasSourceTokenVisit: Boolean;

    { flag set true when a a visitor request that the current item be deleted,
      and the index is thereafter wrong }
    fbRecalcIndex: Boolean;

    fcVisitor: TBaseTreeNodeVisitor;

    procedure InitialiseFlags;
    procedure ProcessAction(const pcNode: TParseTreeNode; const prResults: TRVisitResult);
    procedure VisitTree(const pcNode: TParseTreeNode);

  public
    procedure Visit(const pcRoot: TParseTreeNode; const pcVisitor: TBaseTreeNodeVisitor);
  end;

implementation

uses
  { delphi } SysUtils;

procedure TTreeWalker.InitialiseFlags;
begin
  { read these once only for speed  }

  fbHasPreVisit := fcVisitor.HasPreVisit;
  fbHasPostVisit := fcVisitor.HasPostVisit;
  fbHasSourceTokenVisit := fcVisitor.HasSourceTokenVisit;
end;

procedure TTreeWalker.ProcessAction(const pcNode: TParseTreeNode; const prResults: TRVisitResult);
var
  lcParent: TParseTreeNode;
begin
  case prResults.Action of
    aNone: ;
    aDelete:
    begin
      lcParent := pcNode.Parent;
       // remove current node from parent
      lcParent.RemoveChild(pcNode);
      fbRecalcIndex := True;
    end;
    aInsertBefore:
    begin
      // must have a new item
      Assert(prResults.NewItem <> nil);

      lcParent := pcNode.Parent;

      lcParent.InsertChild(lcParent.IndexOfChild(pcNode),
        TParseTreeNode(prResults.NewItem));
      if prResults.NewItem2 <> nil then
        lcParent.InsertChild(lcParent.IndexOfChild(pcNode),
          TParseTreeNode(prResults.NewItem2));

      fbRecalcIndex := True;
    end;
    else
      Assert(False, 'Unhandled action ' + IntToStr(Ord(prResults.action)));
  end;
end;

procedure TTreeWalker.VisitTree(const pcNode: TParseTreeNode);
const
  { if a node has more than this number of direct children, then something is very wrong
   can have lots in some "header" units that just list a lot of consts }
  MAX_NODE_CHILDREN = 32768;
var
  lrResults: TRVisitResult;
  liLoop: Integer;
  lcChildNode: TParseTreeNode;
  liNewIndex: Integer;
begin
  if pcNode.IsLeaf then
  begin
    if fbHasSourceTokenVisit then
    begin
      ClearVisitResult(lrResults);

      fcVisitor.VisitSourceToken(pcNode, lrResults);
      if lrResults.Action <> aNone then
        ProcessAction(pcNode, lrResults);
    end;
  end
  else
  begin
    { not leaf - visit children }
    if fbHasPreVisit then
    begin
      ClearVisitResult(lrResults);

      fcVisitor.PreVisitParseTreeNode(pcNode, lrResults);
      if lrResults.Action <> aNone then
        ProcessAction(pcNode, lrResults);
    end;


    if pcNode.ChildNodeCount > MAX_NODE_CHILDREN then
    begin
      // some parse or insert process has gone bezerk
      raise Exception.Create('Too many child nodes ' + IntToStr(pcNode.ChildNodeCount));
    end;

    liLoop := 0;
    while liLoop < pcNode.ChildNodeCount do
    begin
      lcChildNode := pcNode.ChildNodes[liLoop];
      VisitTree(lcChildNode);

      { fbRecalcIndex flag is for speed
        need to deal with shifting indexes when an item is moved or deleted
        but only then. The rest of the time it just slows us down }
      if fbRecalcIndex then
      begin
        { has this node been moved or removed?
        if so, don't increment counter, as the next item will now be in this slot }
        liNewIndex := pcNode.IndexOfChild(lcChildNode);
        fbRecalcIndex := False;

        if liNewIndex >= 0 then
          // proceed to next one
          liLoop := liNewIndex + 1;
          { else case is that liNewIndex is -1 as the current item has been deleted.
            Stay at same index as the next item will now be in this slot }
      end
      else
        inc(liLoop);
    end;

    if fbHasPostVisit then
      fcVisitor.PostVisitParseTreeNode(pcNode);
  end;
end;

procedure TTreeWalker.Visit(const pcRoot: TParseTreeNode; const pcVisitor: TBaseTreeNodeVisitor);
begin
  Assert(pcRoot <> nil);
  Assert(pcVisitor <> nil);
  fcVisitor := pcVisitor;

  InitialiseFlags;

  VisitTree(pcRoot);
end;

end.
