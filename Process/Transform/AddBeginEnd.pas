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
  JcfSettings, SourceToken, Tokens, SetTransform, TokenUtils;

function IsBlockParent(const pcNode: TParseTreeNode): boolean;
const
  BLOCK_PARENTS: TParseTreeNodeTypeSet =
    [nIfBlock, nElseBlock, nCaseSelector, nElseCase,
    nWhileStatement, nForStatement, nWithStatement];
begin
  Result := (pcNode <> nil) and (pcNode.NodeType in BLOCK_PARENTS);
end;

function HasBlockChild(const pcNode: TParseTreeNode): boolean;
var
  liDepth: integer;
begin
  if pcNode.NodeType = nElseCase then
    liDepth := 3
  else
    liDepth := 2;

  { a compound statement is the begin..end block. }
  Result := pcNode.HasChildNode(nCompoundStatement, liDepth);
end;

procedure AddBlockChild(const pcNode: TParseTreeNode);
var
  liIndex: integer;
  lcTop: TParseTreeNode;
  lcStatement, lcCompound, lcStatementList: TParseTreeNode;
  lcBegin, lcEnd: TSourceToken;
begin
  { this is an if block or the like
    with a single statement under it  }
  if pcNode.NodeType = nElseCase then
    lcTop := pcNode.GetImmediateChild(nStatementList)
  else
    lcTop := pcNode;

  lcStatement := lcTop.GetImmediateChild(nStatement);

  if lcStatement = nil then
  begin
    // a dangling else or the like
    liIndex := 0;
  end
  else
  begin
    liIndex := lcTop.IndexOfChild(lcStatement);
    Assert(liIndex >= 0);
  end;

  { temporarily take it out }
  lcTop.ExtractChild(lcStatement);

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
    lcTop.InsertChild(liIndex, lcCompound);

    lcBegin := TSourceToken.Create;
    lcBegin.SourceCode := 'begin';
    lcBegin.TokenType := ttBegin;
    lcCompound.AddChild(lcBegin);
    lcCompound.AddChild(NewSpace(1));

    lcStatementList := TParseTreeNode.Create;
    lcStatementList.NodeType := nStatementList;
    lcCompound.AddChild(lcStatementList);

    { the original statement goes in the middle of this }
    if lcStatement <> nil then
      lcStatementList.AddChild(lcStatement);

    lcCompound.AddChild(NewSpace(1));

    lcEnd := TSourceToken.Create;
    lcEnd.SourceCode := 'end';
    lcEnd.TokenType := ttEnd;
    lcCompound.AddChild(lcEnd);
end;

procedure RemoveBlockChild(const pcNode: TParseTreeNode);
var
  lcTop, lcTopStatement: TParseTreeNode;
  lcCompoundStatement: TParseTreeNode;
  lcStatementList: TParseTreeNode;
  lcStatement: TParseTreeNode;
  liIndex: integer;
begin
  if pcNode.NodeType = nElseCase then
    lcTop := pcNode.GetImmediateChild(nStatementList)
  else
    lcTop := pcNode;

  Assert(lcTop <> nil);

  lcTopStatement := lcTop.GetImmediateChild(nStatement);
  if lcTopStatement = nil then
    exit;

  liIndex := lcTop.IndexOfChild(lcTopStatement);
  Assert(liIndex >= 0);

  lcCompoundStatement := lcTopStatement.GetImmediateChild(nCompoundStatement);
  if lcCompoundStatement = nil then
    exit;

  lcStatementList := lcCompoundStatement.GetImmediateChild(nStatementList);
  if lcStatementList = nil then
    exit;

  // if this begin...end owns more than one statement, we can't do it
  if lcStatementList.CountImmediateChild(nStatement) > 1 then
    exit;

  lcStatement := lcStatementList.GetImmediateChild(nStatement);

  if lcStatement <> nil then
  begin
    // right, put this single statement in at the top
    lcStatementList.ExtractChild(lcStatement);
    lcTop.InsertChild(liIndex, lcStatement);
  end;

  // and free the rest of the scaffolding
  lcTopStatement.Free;
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
  begin
    if HasBlockChild(lcNode) then
    begin
      if FormatSettings.Transform.BeginEndStyle = ebNever then
        RemoveBlockChild(lcNode);
    end
    else
    begin
      if FormatSettings.Transform.BeginEndStyle = ebAlways then
        AddBlockChild(lcNode);
    end;
  end;
end;

function TAddBeginEnd.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.Transform.BeginEndStyle <> ebLeave);
end;

end.
