unit VisitParseTree;

{ AFS 28 December 2002

  Define the interface for the visitor pattern
  to visit tree nodes
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is VisitParseTree, released May 2003.
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

type

  // action on visiting a node.
  TVisitAction = (aNone, aDelete, aInsertAfter, aInsertBefore);

  // what happens on visiting a node. More fields to come?
  TRVisitResult = record
    Action: TVisitAction;
    NewItem: TObject;
    NewItem2: TObject;
  end;

  IVisitParseTree = interface

    { there are two kinds of node - interior (parse tree node)
      and leaf (source token) }
    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
    procedure PostVisitParseTreeNode(const pcNode: TObject);
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);

  end;

procedure ClearVisitResult(var prVisitResult: TRVisitResult);

implementation

procedure ClearVisitResult(var prVisitResult: TRVisitResult);
begin
  prVisitResult.Action := aNone;
  prVisitResult.NewItem := nil;
  prVisitResult.NewItem2 := nil;
end;

end.