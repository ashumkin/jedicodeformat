unit ParseTreeNode;

{ AFS 27 October 2002
  this is the start of a new development
   - to give the code formatter a full parse tree
   in the recursive descent mould
   and thereby pave the way for a version 2.0
   and e3ventually take over the world
}

interface

uses
  {delphi } Contnrs,
  { local } WordMap, VisitParseTree, ParseTreeNodeType, TokenType;


type

  TParseTreeNode = class(TObject)
  private
    fcParent: TParseTreeNode;
    fcChildNodes: TObjectList;
    feNodeType: TParseTreeNodeType;

    function GetChildNodes(const piIndex: integer): TParseTreeNode;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function ChildNodeCount: integer;
    function RecursiveChildCount: integer;
    function MaxDepth: integer;

    function NewChild: TParseTreeNode;
    procedure AddChild(const pcChild: TParseTreeNode);
    procedure InsertChild(const piIndex: Integer; const pcChild: TParseTreeNode);
    function RemoveChild(const pcChild: TParseTreeNode): integer;
    function IndexOfChild(const pcChild: TParseTreeNode): integer;
    function SolidChildCount: integer; virtual;

    function FirstLeaf: TParseTreeNode;
    function FirstSolidLeaf: TParseTreeNode; virtual;
    function LastLeaf: TParseTreeNode;
    function PriorLeafNode: TParseTreeNode;
    function NextLeafNode: TParseTreeNode;

    function FirstNodeBefore(const pcChild: TParseTreeNode): TParseTreeNode;
    function FirstNodeAfter(const pcChild: TParseTreeNode): TParseTreeNode;
    function GetImmediateChild(const peNodeTypes: TParseTreeNodeTypeSet): TParseTreeNode;

    function Level: integer;
    function HasChildren: boolean;
    function Root: TParseTreeNode;

    function HasChildNode(const peWords: TWordSet): Boolean; overload; virtual;
    function HasChildNode(const peWords: TWordSet; const piMaxDepth: integer): Boolean; overload; virtual;
    function HasChildNode(const peTokens: TTokenTypeSet; const piMaxDepth: integer): Boolean; overload; virtual;
    function HasChildNode(const peToken: TTokenType; const piMaxDepth: integer): Boolean; overload; virtual;

    function HasParentNode(const peNodeTypes: TParseTreeNodeTypeSet): Boolean; overload;
    function HasParentNode(const peNodeType: TParseTreeNodeType): Boolean; overload;
    function GetParentNode(const peNodeTypes: TParseTreeNodeTypeSet): TParseTreeNode; overload;
    function GetParentNode(const peNodeType: TParseTreeNodeType): TParseTreeNode; overload;

    function Describe: string; virtual;

    procedure AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult); virtual;

    { visit self and all child nodes }
    procedure VisitTree(const pcVisitor: IVisitParseTree);

    property Parent: TParseTreeNode read fcParent write fcParent;
    property ChildNodes[const piIndex: integer]: TParseTreeNode read GetChildNodes;
    property NodeType: TParseTreeNodeType read feNodeType write feNodeType;
  end;


implementation

uses SysUtils, Math;

constructor TParseTreeNode.Create;
begin
  inherited Create;

  fcParent := nil;
  feNodeType := nUnknown;

  fcChildNodes := TObjectList.Create;
  fcChildNodes.OwnsObjects := True;
end;

destructor TParseTreeNode.Destroy;
begin
  FreeAndNil(fcChildNodes);

  inherited;
end;


function TParseTreeNode.ChildNodeCount: integer;
begin
  Result := fcChildNodes.Count;
end;

function TParseTreeNode.GetChildNodes(const piIndex: integer): TParseTreeNode;
begin
  Result := TParseTreeNode(fcChildNodes[piIndex]);
end;

function TParseTreeNode.NewChild: TParseTreeNode;
begin
  // a new child, properly attached parent <-> child
  Result := TParseTreeNode.Create;
  fcChildNodes.Add(Result);
end;


procedure TParseTreeNode.AddChild(const pcChild: TParseTreeNode);
begin
  pcChild.fcParent := self;
  fcChildNodes.Add(pcChild);
end;

procedure TParseTreeNode.InsertChild(const piIndex: Integer; const pcChild: TParseTreeNode);
begin
  pcChild.fcParent := self;
  fcChildNodes.Insert(piIndex, pcChild);
end;

function TParseTreeNode.RemoveChild(const pcChild: TParseTreeNode): integer;
begin
  Result := fcChildNodes.Remove(pcChild);
end;


function TParseTreeNode.IndexOfChild(const pcChild: TParseTreeNode): integer;
begin
  Result := fcChildNodes.IndexOf(pcChild);
end;

{ how far down the tree is this node? }
function TParseTreeNode.Level: integer;
begin
  if fcParent = nil then
    Result := 0
  else
    Result := fcParent.Level + 1;
end;

function TParseTreeNode.HasChildren: boolean;
begin
  Result := (ChildNodeCount > 0);
end;


function TParseTreeNode.Describe: string;
begin
  Result := NodeTypeToString(NodeType);
end;


function TParseTreeNode.MaxDepth: integer;
var
  liLoop: integer;
  liMaxChildDepth, liChildDepth: integer;
begin
  liMaxChildDepth := 0;

  // one deeper than the deepest child
  For liLoop := 0 to ChildNodeCount - 1 do
  begin
    liChildDepth := ChildNodes[liLoop].MaxDepth;

    liMaxChildDepth := Max(liMaxChildDepth, liChildDepth);
  end;

  Result := liMaxChildDepth + 1;
end;

function TParseTreeNode.RecursiveChildCount: integer;
var
  liLoop: integer;
begin
  // I am one, and my children are the rest

  Result := 1;

  For liLoop := 0 to ChildNodeCount - 1 do
    Result := Result + ChildNodes[liLoop].RecursiveChildCount;
end;

function TParseTreeNode.Root: TParseTreeNode;
begin
  // if I have a parent then I am not the root
  if (fcParent = nil) then
    Result := self
  else
    Result := fcParent.Root;
end;

function TParseTreeNode.HasChildNode(const peWords: TWordSet): Boolean;
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := 0 to ChildNodeCount - 1 do
  begin
    Result := ChildNodes[liLoop].HasChildNode(peWords);
    if Result then
      break;
  end;
end;

function TParseTreeNode.HasChildNode(const peWords: TWordSet; const piMaxDepth: integer): Boolean;
var
  liLoop: integer;
begin
  Result := False;

  if (piMaxDepth > 0) then
  begin
    for liLoop := 0 to ChildNodeCount - 1 do
    begin
      Result := ChildNodes[liLoop].HasChildNode(peWords, piMaxDepth - 1);
      if Result then
        break;
    end;
  end;
end;

function TParseTreeNode.HasChildNode(const peTokens: TTokenTypeSet; const piMaxDepth: integer): Boolean;
var
  liLoop: integer;
begin
  Result := False;

  if (piMaxDepth > 0) then
  begin
    for liLoop := 0 to ChildNodeCount - 1 do
    begin
      Result := ChildNodes[liLoop].HasChildNode(peTokens, piMaxDepth - 1);
      if Result then
        break;
    end;
  end;
end;

function TParseTreeNode.HasChildNode(const peToken: TTokenType; const piMaxDepth: integer): Boolean;
begin
  Result := HasChildNode([peToken], piMaxDepth);
end;

function TParseTreeNode.HasParentNode(const peNodeTypes: TParseTreeNodeTypeSet): Boolean;
begin
  Result := (NodeType in peNodeTypes);

  // try above
  if (not Result) and (Parent <> nil) then
    Result := Parent.HasParentNode(peNodeTypes);
end;

function TParseTreeNode.HasParentNode(const peNodeType: TParseTreeNodeType): Boolean;
begin
  Result := HasParentNode([peNodeType]);
end;


function TParseTreeNode.GetParentNode(const peNodeTypes: TParseTreeNodeTypeSet): TParseTreeNode;
begin
  if (NodeType in peNodeTypes) then
  begin
    Result := self;
  end
  else
  begin
    Result := nil;

    // try above
    if (Parent <> nil) then
      Result := Parent.GetParentNode(peNodeTypes);
  end;
end;

function TParseTreeNode.GetParentNode(const peNodeType: TParseTreeNodeType): TParseTreeNode;
begin
  Result := GetParentNode([peNodeType]);
end;



procedure TParseTreeNode.AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult);
begin
  Assert(pcVisitor <> nil);
  pcVisitor.VisitParseTreeNode(self, prVisitResults);
end;

procedure TParseTreeNode.VisitTree(const pcVisitor: IVisitParseTree);
var
  liLoop, liNewIndex: integer;
  lcNode: TParseTreeNode;
  lrVisitResult: TRVisitResult;
begin
  ClearVisitResult(lrVisitResult);
  AcceptVisitor(pcVisitor, lrVisitResult);
  // process the results

  case lrVisitResult.action of
    aNone: ;
    aDelete:
    begin
      // remove self - do it via the parent
      Parent.RemoveChild(self);
      // can't go on here, no more self
      exit;
    end;
    aInsertAfter:
    begin
      // must have a new item
      Assert(lrVisitResult.NewItem <> nil);
      Parent.InsertChild(Parent.IndexOfChild(self) + 1,  TParseTreeNode(lrVisitResult.NewItem));
    end
    else
      Assert(false, 'Unhandled action ' + IntToStr(Ord(lrVisitResult.action)));
  end;


  liLoop := 0;
  while liLoop < ChildNodeCount do
  begin
    lcNode := ChildNodes[liLoop];
    lcNode.VisitTree(pcVisitor);

    { has this node been removed?
      if so, don't increment counter, as the next item will now be in this slot }
    liNewIndex := IndexOfChild(lcNode);

    if liNewIndex <> liLoop then
    begin
      { it has moved during processing. }
      if liNewIndex < 0 then
      begin
        { deleted. Nothing to do.
          Don't even inc the loop counter
          as the next item will now be in this slot
        }
      end
      else
        Assert(False);
    end
    else
      inc(liLoop);
  end;
end;

function TParseTreeNode.FirstLeaf: TParseTreeNode;
begin
  if ChildNodeCount = 0 then
    Result := self // I am a leaf
  else
    Result := ChildNodes[0].FirstLeaf; // go down
end;

function TParseTreeNode.FirstSolidLeaf: TParseTreeNode;
var
  liLoop: integer;
begin
  Result := nil;

  for liLoop := 0 to ChildNodeCount - 1 do
  begin
    Result := ChildNodes[liLoop].FirstSolidLeaf; // go down
    if Result <> nil then
      break;
  end;
end;

function TParseTreeNode.LastLeaf: TParseTreeNode;
begin
  if ChildNodeCount = 0 then
    Result := self // I am a leaf
  else
    Result := ChildNodes[ChildNodeCount - 1].FirstLeaf; // go down
end;

{ find the first leaf before this one }
function TParseTreeNode.PriorLeafNode: TParseTreeNode;
var
  lcFocus, lcParent: TParseTreeNode;
begin
  // get the node before this one
  Result := Parent.FirstNodeBefore(Self);

  if Result = nil then
  begin
    { climb the tree until we reach the top or a node with stuff before this }
    lcParent := Parent;

    While (Result = nil) and (lcParent <> nil) do
    begin
      lcFocus := lcParent;
      lcParent := lcParent.Parent;

      if lcParent <> nil then
        Result := lcParent.FirstNodeBefore(lcFocus);
    end;
  end;

  // result may not be a leaf node
  if Result <> nil then
    Result := Result.LastLeaf;
end;

function TParseTreeNode.NextLeafNode: TParseTreeNode;
var
  lcFocus, lcParent: TParseTreeNode;
begin
  // get the node after this one
  Result := Parent.FirstNodeAfter(Self);

  if Result = nil then
  begin
    { climb the tree until we reach the top or a node with stuff before this }
    lcParent := Parent;

    While (Result = nil) and (lcParent <> nil) do
    begin
      lcFocus := lcParent;
      lcParent := lcParent.Parent;

      Result := lcParent.FirstNodeAfter(lcFocus);
    end;
  end;

  // result may not be a leaf node
  if Result <> nil then
    Result := Result.FirstLeaf;
end;

function TParseTreeNode.FirstNodeBefore(const pcChild: TParseTreeNode): TParseTreeNode;
var
  liIndex: integer;
begin

  liIndex := IndexOfChild(pcChild);
  if liIndex > 0 then
    Result := ChildNodes[liIndex - 1]
  else
    Result := nil;
end;

function TParseTreeNode.FirstNodeAfter(const pcChild: TParseTreeNode): TParseTreeNode;
var
  liIndex: integer;
begin

  liIndex := IndexOfChild(pcChild);
  if liIndex < (ChildNodeCount - 1) then
    Result := ChildNodes[liIndex + 1]
  else
    Result := nil;
end;

function TParseTreeNode.SolidChildCount: integer;
var
  liLoop: integer;
begin
  Result := 0;

  for liLoop := 0 to ChildNodeCount - 1 do
  begin
    Result := Result + ChildNodes[liLoop].SolidChildCount;
  end;

end;

function TParseTreeNode.GetImmediateChild(const peNodeTypes: TParseTreeNodeTypeSet): TParseTreeNode;
var
  liLoop: integer;
  lcNode: TParseTreeNode;
begin
  Result := nil;

  for liLoop := 0 to ChildNodeCount -1 do
  begin
    lcNode := ChildNodes[liLoop];

    if lcNode.NodeType in peNodeTypes then
    begin
      Result := lcNode;
      break;
    end;
  end;
end;

end.
