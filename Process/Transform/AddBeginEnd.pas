unit AddBeginEnd;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AddBeginEnd.pas, March 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2004 Anthony Steele.
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

uses BaseVisitor;

type
  TAddBeginEnd = class(TBaseTreeNodeVisitor)
  private

  protected
  public
    constructor Create; override;

    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses ParseTreeNode, ParseTreeNodeType,
  SourceToken, Tokens;

function IsBlockParent(const pcNode: TParseTreeNode): boolean;
begin
  Result := False;

  { a block for
    - if block
    - else block
    - case label block
  }
  if pcNode.NodeType in [nIfBlock, nElseBlock, nCaseSelector,
    nWhileStatement, nForStatement] then
    Result := True;

  { while, do, for statements etc }
end;

function HasBlockChild(const pcNode: TParseTreeNode): boolean;
begin
  Result := False;

  // an if statement has a block
  if pcNode.HasChildNode(nCompoundStatement, 2) then
    Result := True;
end;

procedure AddBlockChild(const pcNode: TParseTreeNode);
var
  liIndex: integer;
  lcStatement, lcCompound, lcStatementList: TParseTreeNode;
  lcBegin, lcEnd: TSourceToken;
begin
  { this is an if block or the like
    with a single statement under it
  }

  lcStatement := pcNode.GetImmediateChild(nStatement);
  Assert(lcStatement <> nil);
  liIndex := pcNode.IndexOfChild(lcStatement);

  { temporarily take it out }
  pcNode.ExtractChild(lcStatement);

  { need some new nodes:
    statement
     - compound statement
       - begin
       - statement list
         -  lcStatement
       - end

    }
    lcCompound := TParseTreeNode.Create;
    lcCompound.NodeType := nCompoundStatement;
    pcNode.InsertChild(liIndex, lcCompound);

    lcBegin := TSourceToken.Create;
    lcBegin.SourceCode := 'begin';
    lcBegin.TokenType := ttBegin;
    lcCompound.AddChild(lcBegin);

    lcStatementList := TParseTreeNode.Create;
    lcStatementList.NodeType := nStatementList;
    lcCompound.AddChild(lcStatementList);

    { the original statement goes in the middle of this }
    lcStatementList.AddChild(lcStatement);

    lcEnd := TSourceToken.Create;
    lcEnd.SourceCode := 'end';
    lcEnd.TokenType := ttEnd;
    lcCompound.AddChild(lcEnd);
end;

constructor TAddBeginEnd.Create;
begin
  inherited;

  HasPostVisit := True;
  HasSourceTokenVisit := False;
end;

procedure TAddBeginEnd.PostVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  if IsBlockParent(lcNode) then
    if not HasBlockChild(lcNode) then
      AddBlockChild(lcNode);

end;


function TAddBeginEnd.IsIncludedInSettings: boolean;
begin
  // todo - setting
  Result := True;
end;

end.
