unit fShowParseTree;

{
  AFS 2002

  A form to show a unit's parse tree
  mainly for debugiing purposes when the parse goes wrong
}


interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  { local }
  ParseTreeNode;

type
  TfrmShowParseTree = class(TForm)
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
    pcPages: TPageControl;
    tsTokens: TTabSheet;
    tsTree: TTabSheet;
    tvParseTree: TTreeView;
    lvTokens: TListView;
    procedure tvParseTreeChange(Sender: TObject; Node: TTreeNode);
    procedure cbShowWhiteSpaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvTokensClick(Sender: TObject);
    procedure lvTokensSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    fcRootNode: TParseTreeNode;
    procedure ShowTreeNodeDetails(const pcNode: TParseTreeNode);
  public
    property RootNode: TParseTreeNode read fcRootNode write fcRootNode;

    procedure DisplayTree;
  end;

procedure ShowParseTree(const pcRoot: TParseTreeNode);

implementation

{$R *.dfm}

uses SourceToken, TokenType, WordMap;

procedure ShowParseTree(const pcRoot: TParseTreeNode);
var
  lfParseTree: TfrmShowParseTree;
begin
  Assert(pcRoot <> nil);

  lfParseTree := TfrmShowParseTree.Create(Application);
  try
    lfParseTree.RootNode := pcRoot;
    lfParseTree.DisplayTree;

    lfParseTree.ShowModal;

  finally
    lfParseTree.Free;
  end;
end;



procedure TfrmShowParseTree.DisplayTree;

  procedure ShowTokensInList(const pcData: TParseTreeNode);
  var
    lcNewItem: TListItem;
    lcToken: TSourceToken;
    liLoop: integer;
    lsDesc: string;
  begin
    { exclude this one as white space }
    if (not cbShowWhiteSpace.Checked) and (not pcData.HasChildren) and
      (pcData is TSourceToken) and (TSourceToken(pcData).TokenType in NotSolidTokens) then
        exit;

    { list tokens }
    if (pcData is TSourceToken) then
    begin
      lcToken := TSourceToken(pcData);

      lcNewItem := lvTokens.Items.Add;
      lcNewItem.Caption := IntToStr(lvTokens.Items.Count);

      lsDesc := TokenTypeToString(lcToken.TokenType);
      if lcToken.Word <> wUnknown then
        lsDesc := lsDesc + ' ' + WordToString(lcToken.Word);
      lcNewItem.SubItems.Add(lsDesc);
      lcNewItem.SubItems.Add(lcToken.SourceCode);

       lcNewItem.Data := pcData;
    end;

    // attach the children
    for liLoop := 0 to  pcData.ChildNodeCount - 1 do
      ShowTokensInList(pcData.ChildNodes[liLoop]);
  end;

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
  lblTreeCount.Caption := 'Tree has ' + IntToStr(fcRootNode.RecursiveChildCount) + ' nodes';
  lblTreeDepth.Caption := 'Tree has max depth of ' + IntToStr(fcRootNode.MaxDepth);


  lvTokens.Items.BeginUpdate;
  try
    lvTokens.Items.Clear;

    ShowTokensInList(fcRootNode);

  finally
    lvTokens.Items.EndUpdate;
  end;

  tvParseTree.Items.BeginUpdate;
  try
    tvParseTree.Items.Clear;
    MakeNodeChildren(nil, fcRootNode);

    tvParseTree.FullExpand;
  finally
    tvParseTree.Items.EndUpdate;
  end;

end;

procedure TfrmShowParseTree.tvParseTreeChange(Sender: TObject;  Node: TTreeNode);
begin
  if Node = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(Node.Data);
end;

procedure TfrmShowParseTree.ShowTreeNodeDetails(const pcNode: TParseTreeNode);
begin

  if pcNode = nil then
  begin
    lblCurrent.Caption := 'Current: none';
    lblDepth.Caption := 'Depth: -';
    lblImmediateChildCount.Caption := 'Immediate child count: -';
    lblTotalNodeCount.Caption := 'Total node count: -';
  end
  else
  begin
    lblCurrent.Caption := 'Current: ' + pcNode.Describe;
    lblDepth.Caption := 'Level: ' + IntToStr(pcNode.Level);
    lblImmediateChildCount.Caption := 'Immediate child count: ' + IntToStr(pcNode.ChildNodeCount);
    lblTotalNodeCount.Caption := 'Total node count: ' + IntToStr(pcNode.RecursiveChildCount);
  end;
end;

procedure TfrmShowParseTree.cbShowWhiteSpaceClick(Sender: TObject);
begin
  // ShowWhiteSpace setting has changed. Redisplay
  DisplayTree;
end;

procedure TfrmShowParseTree.FormShow(Sender: TObject);
begin
  pcPages.ActivePage := tsTree;
end;

procedure TfrmShowParseTree.lvTokensClick(Sender: TObject);
begin
  if lvTokens.Selected = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(lvTokens.Selected.Data);
end;

procedure TfrmShowParseTree.lvTokensSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if lvTokens.Selected = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(lvTokens.Selected.Data);
end;

end.
