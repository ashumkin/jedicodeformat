unit WarnEmptyBlock;

{ AFS 30 Dec 2002
 warn of an enmpty block, one of
 begin..end, try..except, try..finally, except..end, finally..end
}


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is WarnEmptyBlock, released May 2003.
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

uses Warning;

type

  TWarnEmptyBlock = class(TWarning)
  public
    constructor Create; override;

    function PreVisitParseTreeNode(const pcNode: TObject): Boolean; override;
  end;

implementation

uses ParseTreeNode, ParseTreeNodeType, BaseVisitor;

constructor TWarnEmptyBlock.Create;
begin
  inherited;

  HasPreVisit := True;
  HasPostVisit := False;
  HasSourceTokenVisit := False;
end;

function TWarnEmptyBlock.PreVisitParseTreeNode(const pcNode: TObject): Boolean;
var
  lcNode: TParseTreeNode;
  liSolidChildCount: integer;
begin
  Result := False;
  lcNode := TParseTreeNode(pcNode);

  // only look in statements
  if not lcNode.HasParentNode(nBlock) then
    exit;

  { looking for nodes with 2 solid tokens under them
    e.g. 'begin' and 'end'
  }
  liSolidChildCount := lcNode.SolidChildCount;

  if liSolidChildCount = 2 then
  begin
    if lcNode.NodeType = nCompoundStatement then
    begin
      SendWarning(lcNode, 'Empty begin..end block');
    end;

    if lcNode.NodeType = nFinallyBlock then
    begin
      SendWarning(lcNode, 'Empty finally..end block');
    end;

    if lcNode.NodeType = nExceptBlock then
    begin
      SendWarning(lcNode, 'Empty except..end block');
    end;
  end
  else if liSolidChildCount = 1 then
  begin
    if lcNode.NodeType = nTryBlock then
    begin
      SendWarning(lcNode, 'Empty try block');
    end;
  end;

end;

end.
