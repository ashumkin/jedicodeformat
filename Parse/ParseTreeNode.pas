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
  { local } WordMap;

type

  TParseTreeNode = class(TObject)
  private
    fcParent: TParseTreeNode;
    fcChildNodes: TObjectList;
    fsName: string;

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

    function Level: integer;
    function HasChildren: boolean;
    function Root: TParseTreeNode;
    function HasChildNode(const peWords: TWordSet): Boolean; virtual;

    function Describe: string; virtual;

    property Parent: TParseTreeNode read fcParent write fcParent;
    property ChildNodes[const piIndex: integer]: TParseTreeNode read GetChildNodes;
    property Name: string read fsName write fsName;
  end;

implementation

uses SysUtils, Math;

constructor TParseTreeNode.Create;
begin
  inherited Create;

  fcParent := nil;
  fcChildNodes := TObjectList.Create;
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
  Result := Name;
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

end.
