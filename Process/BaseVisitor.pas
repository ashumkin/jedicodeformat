unit BaseVisitor;

{ AFS 28 Dec 2002

  Base class that implments the tree node Visitor interface
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is BaseVisitor, released May 2003.
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

uses VisitParseTree;

type

  TBaseTreeNodeVisitor = class(TInterfacedObject, IVisitParseTree)

  public
    constructor Create; virtual;

    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); virtual;
    procedure PostVisitParseTreeNode(const pcNode: TObject); virtual;
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); virtual;

    function FinalSummary(var psMessage: string): Boolean; virtual;
    function IsIncludedInSettings: boolean; virtual;
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

function TBaseTreeNodeVisitor.IsIncludedInSettings: boolean;
begin
  // here for override
  Result := True;
end;

end.