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
  { local } WordMap, VisitParseTree, ParseTreeNodeType, TokenType, Nesting;


type

  TParseTreeNode = class(TObject)
  private
    fcParent: TParseTreeNode;
    fcChildNodes: TObjectList;
    feNodeType: TParseTreeNodeType;
    fcNestings: TNestingLevelList;

    function GetChildNodes(const piIndex: integer): TParseTreeNode;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function ChildNodeCount: integer;
    function RecursiveChildCount: integer;
    function MaxDepth: integer;
    function IndexOfSelf: integer;

    function NewChild: TParseTreeNode;
    procedure AddChild(const pcChild: TParseTreeNode);
    procedure InsertChild(const piIndex: Integer; const pcChild: TParseTreeNode);
    function RemoveChild(const pcChild: TParseTreeNode): integer; overload;
    function RemoveChild(const piIndex: integer): integer; overload;

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
    function IsLeaf: Boolean; virtual;
    function Root: TParseTreeNode;

    function HasChildNode(const peTokens: TTokenTypeSet): Boolean; overload; virtual;
    function HasChildNode(const peWords: TWordSet): Boolean; overload; virtual;
    function HasChildNode(const peWords: TWordSet; const piMaxDepth: integer): Boolean; overload; virtual;
    function HasChildNode(const peTokens: TTokenTypeSet; const piMaxDepth: integer): Boolean; overload; virtual;
    function HasChildNode(const peToken: TTokenType; const piMaxDepth: integer): Boolean; overload; virtual;

    function HasParentNode(const peNodeTypes: TParseTreeNodeTypeSet): Boolean; overload;
    function HasParentNode(const peNodeType: TParseTreeNodeType): Boolean; overload;

    function HasParentNode(const peNodeTypes: TParseTreeNodeTypeSet; const piMaxDepth: integer): Boolean; overload;
    function HasParentNode(const peNodeType: TParseTreeNodeType; const piMaxDepth: integer): Boolean; overload;

    function GetParentNode(const peNodeTypes: TParseTreeNodeTypeSet): TParseTreeNode; overload;
    function GetParentNode(const peNodeType: TParseTreeNodeType): TParseTreeNode; overload;


    { this one needs some explanation. Need to answer questions like
     'Is this node in a type decl, on the right of an equal sign
     So we find if we have a predecessor of one of peWords in the subtree rooted at peNodeTypes }
    function IsOnRightOf(const peRootNodeTypes: TParseTreeNodeTypeSet; const peWords: TWordSet): Boolean; overload;
    function IsOnRightOf(const peRootNodeType: TParseTreeNodeType; const peWord: TWord): Boolean; overload;

    { same with token types}
    function IsOnRightOf(const peRootNodeTypes: TParseTreeNodeTypeSet; const peTokens: TTokenTypeSet): Boolean; overload;
    function IsOnRightOf(const peRootNodeType: TParseTreeNodeType; const peToken: TTokenType): Boolean; overload;

    { same with parse tree interior nodes }
    function IsOnRightOf(const peRootNodeTypes, peNodes: TParseTreeNodeTypeSet): Boolean; overload;
    function IsOnRightOf(const peRootNodeType, peNode: TParseTreeNodeType): Boolean; overload;


    function Describe: string; virtual;

    procedure AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult); virtual;

    { visit self and all child nodes }
    procedure VisitTree(const pcVisitor: IVisitParseTree);

    property Parent: TParseTreeNode read fcParent write fcParent;
    property ChildNodes[const piIndex: integer]: TParseTreeNode read GetChildNodes;
    property NodeType: TParseTreeNodeType read feNodeType write feNodeType;

    property Nestings: TNestingLevelList read fcNestings;
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

  fcNestings := TNestingLevelList.Create;
end;

destructor TParseTreeNode.Destroy;
begin
  FreeAndNil(fcChildNodes);
  FreeANdNil(fcNestings);

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

function TParseTreeNode.RemoveChild(const piIndex: integer): integer;
begin
  if (piIndex < 0) or (piIndex >= ChildNodeCount) then
  begin
    Result := -1;
  end
  else
    Result := fcChildNodes.Remove(fcChildNodes[piIndex]);
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

function TParseTreeNode.IsLeaf: Boolean;
begin
  Result := False;
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

function TParseTreeNode.HasChildNode(const peTokens: TTokenTypeSet): Boolean;
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := 0 to ChildNodeCount - 1 do
  begin
    Result := ChildNodes[liLoop].HasChildNode(peTokens);
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

function TParseTreeNode.HasParentNode(const peNodeTypes: TParseTreeNodeTypeSet; const piMaxDepth: integer): Boolean;
begin
  Result := (NodeType in peNodeTypes);

  // try above
  if (not Result) and (Parent <> nil) and (piMaxDepth > 0) then
    Result := Parent.HasParentNode(peNodeTypes, (piMaxDepth - 1));
end;

function TParseTreeNode.HasParentNode(const peNodeType: TParseTreeNodeType; const piMaxDepth: integer): Boolean;
begin
  Result := HasParentNode([peNodeType], piMaxDepth);
end;


function TParseTreeNode.GetParentNode(const peNodeType: TParseTreeNodeType): TParseTreeNode;
begin
  Result := GetParentNode([peNodeType]);
end;

function TParseTreeNode.IsOnRightOf(const peRootNodeTypes: TParseTreeNodeTypeSet; const peWords: TWordSet): Boolean;
var
  lbSearchDone: Boolean;

  function GetFirstMatch(const pcRoot: TParseTreeNode; const peWords: TWordSet): TParseTreeNode;
  var
    liLoop: integer;
    lcChild: TParseTreeNode;
  begin
    Result := nil;

    if pcRoot = self then
    begin
      lbSearchDone := True;
      exit;
    end;

    // leaf node - matching token using the 'HasChildNode' override to match self
    if (ChildNodeCount = 0) and HasChildNode(peWords) then
    begin
      lbSearchDone := True;
      Result := self;
      exit;
    end;

    // recurse into all children (or until self is encountered)
    for liLoop := 0 to ChildNodeCount - 1 do
    begin
      lcChild := ChildNodes[liLoop];
      if lcChild = self then
      begin
        lbSearchDone := True;
        break;
      end;

      Result := GetFirstMatch(lcChild, peWords);
      if Result <> nil then
        break;

      if lbSearchDone then
        break;
   end;
  end;

var
  lcRoot, lcFirstMatch: TParseTreeNode;
begin
  { does it have the required parent }
  lcRoot := GetParentNode(peRootNodeTypes);
  if lcRoot = nil then
  begin
    Result := False;
    exit;
  end;

  { does the parent have the required child
    search depth-first, ending when the self node is reached }

  lbSearchDone := False;
  lcFirstMatch := GetFirstMatch(lcRoot, peWords);

  // not enough - must be before self
  Result := (lcFirstMatch <> nil);
end;

function TParseTreeNode.IsOnRightOf(const peRootNodeType: TParseTreeNodeType; const peWord: TWord): Boolean;
begin
  Result := IsOnRightOf([peRootNodeType], [peWord]);
end;


{ a copy of the above with different types }
function TParseTreeNode.IsOnRightOf(const peRootNodeTypes: TParseTreeNodeTypeSet;
  const peTokens: TTokenTypeSet): Boolean;
var
  lbSearchDone: Boolean;

  function GetFirstMatch(const pcRoot: TParseTreeNode; const peTokens: TTokenTypeSet): TParseTreeNode;
  var
    liLoop: integer;
    lcChild: TParseTreeNode;
  begin
    Result := nil;

    if pcRoot = self then
    begin
      lbSearchDone := True;
      exit;
    end;

    // leaf node - matching token using the 'HasChildNode' override to match self
    if (ChildNodeCount = 0) and HasChildNode(peTokens) then
    begin
      lbSearchDone := True;
      Result := self;
      exit;
    end;

    // recurse into all children (or until self is encountered)
    for liLoop := 0 to ChildNodeCount - 1 do
    begin
      lcChild := ChildNodes[liLoop];
      if lcChild = self then
      begin
        lbSearchDone := True;
        break;
      end;

      Result := GetFirstMatch(lcChild, peTokens);
      if Result <> nil then
        break;

      if lbSearchDone then
        break;
   end;
  end;

var
  lcRoot, lcFirstMatch: TParseTreeNode;
begin
  { does it have the required parent }
  lcRoot := GetParentNode(peRootNodeTypes);
  if lcRoot = nil then
  begin
    Result := False;
    exit;
  end;

  { does the parent have the required child
    search depth-first, ending when the self node is reached  }

  lbSearchDone := False;
  lcFirstMatch := GetFirstMatch(lcRoot, peTokens);

  // not enough - must be before self
  Result := (lcFirstMatch <> nil);
end;

function TParseTreeNode.IsOnRightOf(const peRootNodeType: TParseTreeNodeType;
  const peToken: TTokenType): Boolean;
begin
  Result := IsOnRightOf([peRootNodeType], [peToken]);
end;


function TParseTreeNode.IsOnRightOf(const peRootNodeTypes, peNodes: TParseTreeNodeTypeSet): Boolean;
var
  lbSearchDone: Boolean;

  function GetFirstMatch(const pcRoot: TParseTreeNode; const peNodes: TParseTreeNodeTypeSet): TParseTreeNode;
  var
    liLoop: integer;
    lcChild: TParseTreeNode;
  begin
    Result := nil;

    if pcRoot = self then
    begin
      lbSearchDone := True;
      exit;
    end;

    if pcRoot.NodeType in peNodes then
    begin
      lbSearchDone := True;
      Result := self;
      exit;
    end;

    // recurse into all children (or until self is encountered)
    for liLoop := 0 to ChildNodeCount - 1 do
    begin
      lcChild := ChildNodes[liLoop];
      if lcChild = self then
      begin
        lbSearchDone := True;
        break;
      end;

      Result := GetFirstMatch(lcChild, peNodes);
      if Result <> nil then
        break;

      if lbSearchDone then
        break;
   end;
  end;

var
  lcRoot, lcFirstMatch: TParseTreeNode;
begin
  { does it have the required parent }
  lcRoot := GetParentNode(peRootNodeTypes);
  if lcRoot = nil then
  begin
    Result := False;
    exit;
  end;

  { does the parent have the required child
    search depth-first, ending when the self node is reached  }

  lbSearchDone := False;
  lcFirstMatch := GetFirstMatch(lcRoot, peNodes);

  // not enough - must be before self
  Result := (lcFirstMatch <> nil);
end;

function TParseTreeNode.IsOnRightOf(const peRootNodeType,  peNode: TParseTreeNodeType): Boolean;
begin
  Result := IsOnRightOf([peRootNodeType], [peNode])
end;

procedure TParseTreeNode.AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult);
begin
  Assert(pcVisitor <> nil);
  pcVisitor.PreVisitParseTreeNode(self, prVisitResults);
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
      if lrVisitResult.NewItem2 <> nil then
        Parent.InsertChild(Parent.IndexOfChild(self) + 2,  TParseTreeNode(lrVisitResult.NewItem2));
    end;
    aInsertBefore:
    begin
      // must have a new item
      Assert(lrVisitResult.NewItem <> nil);

      Parent.InsertChild(Parent.IndexOfChild(self),  TParseTreeNode(lrVisitResult.NewItem));
      if lrVisitResult.NewItem2 <> nil then
        Parent.InsertChild(Parent.IndexOfChild(self),  TParseTreeNode(lrVisitResult.NewItem2));
    end;
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

    if liNewIndex >= 0 then
     // proceed to next one
      liLoop := liNewIndex + 1;
    { else case is that liNewIndex is -1 as the current item has been deleted.
      Stay at same index as the next item will now be in this slot }
  end;

  pcVisitor.PostVisitParseTreeNode(self);
end;

function TParseTreeNode.FirstLeaf: TParseTreeNode;
var
  liLoop: integer;
begin
  if IsLeaf then
    Result := Self // I am a leaf
  else if ChildNodeCount = 0 then
  begin
    Result := nil // I am a bare branch.
  end
  else
  begin
    // child may be a bare branch. Look back until a non-bare child is found
    liLoop := 0;
    Result := nil;
    while (Result = nil) and (liLoop < ChildNodeCount) do
    begin
      Result := ChildNodes[liLoop].FirstLeaf; // go down
      inc(liLoop)
    end;
  end;
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
var
  liLoop: integer;
begin
  if IsLeaf then
    Result := Self // I am a leaf
  else if ChildNodeCount = 0 then
  begin
    Result := nil // I am a bare branch.
  end
  else
  begin
    // child may be a bare branch. Look back until a non-bare child is found
    liLoop := ChildNodeCount - 1;
    Result := nil;
    while (Result = nil) and (liLoop >= 0) do
    begin
      Result := ChildNodes[liLoop].LastLeaf; // go down
      dec(liLoop)
    end;
  end;
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
  begin
    if (Result.ChildNodeCount = 0) and (not Result.IsLeaf) then
      // result is a bare branch. Move on
      Result := Result.PriorLeafNode
    else
      Result := Result.LastLeaf;
  end;
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

    while (Result = nil) and (lcParent <> nil) do
    begin
      lcFocus := lcParent;
      lcParent := lcParent.Parent;

      if lcParent <> nil then
        Result := lcParent.FirstNodeAfter(lcFocus);
    end;
  end;

  // result may not be a leaf node
  if Result <> nil then
  begin
    if (Result.ChildNodeCount = 0) and (not Result.IsLeaf) then
      // result is a bare branch. Move on
      Result := Result.NextLeafNode
    else
      Result := Result.FirstLeaf;
  end;
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
  if (liIndex < (ChildNodeCount - 1)) then
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

function TParseTreeNode.IndexOfSelf: integer;
begin
  if Parent = nil then
    Result := 0
  else
    Result := Parent.IndexOfChild(self); 
end;

end.
