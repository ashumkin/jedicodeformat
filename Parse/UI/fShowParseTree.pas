unit fShowParseTree;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  { local }
  ParseTreeNode;

type
  TfrmShowParseTree = class(TForm)
    tvParseTree: TTreeView;
    StatusBar1: TStatusBar;
    pnlTop: TPanel;
    lblTreeCount: TLabel;
    lblTreeDepth: TLabel;
    pnlBottom: TPanel;
    lblCurrent: TLabel;
    lblDepth: TLabel;
    lblTotalNodeCount: TLabel;
    lblImmediateChildCount: TLabel;
    cbShowWhiteSpace: TCheckBox;
    procedure tvParseTreeChange(Sender: TObject; Node: TTreeNode);
    procedure cbShowWhiteSpaceClick(Sender: TObject);
  private
    fcRootNode: TParseTreeNode;
  public
    property RootNode: TParseTreeNode read fcRootNode write fcRootNode;

    procedure MakeTree;
  end;

procedure ShowParseTree(const pcRoot: TParseTreeNode);

implementation

{$R *.dfm}

uses SourceToken, TokenType;

procedure ShowParseTree(const pcRoot: TParseTreeNode);
var
  lfParseTree: TfrmShowParseTree;
begin
  lfParseTree := TfrmShowParseTree.Create(Application);
  try
    lfParseTree.RootNode := pcRoot;
    lfParseTree.MakeTree;

    lfParseTree.ShowModal;

  finally
    lfParseTree.Free;
  end;
end;



procedure TfrmShowParseTree.MakeTree;

  procedure MakeNodeChildren(const pcGUIParent: TTreeNode; const pcData: TParseTreeNode);
  var
    lcNewItem: TTreeNode;
    liLoop: integer;
  begin
    { exclude this one as white space }
    if (not cbShowWhiteSpace.Checked) and (not pcData.HasChildren) and
      (pcData is TSourceToken) and (TSourceToken(pcData).TokenType in NotSolidTokens) then
        exit;

    lcNewItem := tvParseTree.Items.AddChild(pcGUIParent, pcData.Describe);
    lcNewItem.Data := pcData;

    // attach the children
    for liLoop := 0 to  pcData.ChildNodeCount - 1 do
      MakeNodeChildren(lcNewItem, pcData.ChildNodes[liLoop]);
  end;

begin
  tvParseTree.Items.BeginUpdate;
  try
    tvParseTree.Items.Clear;
    MakeNodeChildren(nil, fcRootNode);

    lblTreeCount.Caption := 'Tree has ' + IntToStr(fcRootNode.RecursiveChildCount) + ' nodes';
    lblTreeDepth.Caption := 'Tree has max depth of ' + IntToStr(fcRootNode.MaxDepth);

    tvParseTree.FullExpand;
  finally
    tvParseTree.Items.EndUpdate;
  end;
end;

procedure TfrmShowParseTree.tvParseTreeChange(Sender: TObject;  Node: TTreeNode);
var
  lcParseTreeNode: TParseTreeNode;
begin
  if Node = nil then
  begin
    lblCurrent.Caption := 'Current: none';
    lblDepth.Caption := 'Depth: -';
    lblImmediateChildCount.Caption := 'Immediate child count: -';
    lblTotalNodeCount.Caption := 'Total node count: -';
  end
  else
  begin
    lcParseTreeNode := Node.Data;

    lblCurrent.Caption := 'Current: ' + lcParseTreeNode.Describe;
    lblDepth.Caption := 'Level: ' + IntToStr(lcParseTreeNode.Level);
    lblImmediateChildCount.Caption := 'Immediate child count: ' + IntToStr(lcParseTreeNode.ChildNodeCount);
    lblTotalNodeCount.Caption := 'Total node count: ' + IntToStr(lcParseTreeNode.RecursiveChildCount);
  end;
end;

procedure TfrmShowParseTree.cbShowWhiteSpaceClick(Sender: TObject);
begin
  // ShowWhiteSpace setting has changed. Redisplay
  MakeTree;
end;

end.
