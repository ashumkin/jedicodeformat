unit VisitSetNesting;

{ visitor to set up nesting levels }

interface

uses
  { delphi }
  Contnrs,
  { local }
  BaseVisitor, VisitParseTree, Nesting;

type

  TVisitSetNestings = class(TBaseTreeNodeVisitor)
    private
      fcRunningTotals: TNestingLevelList;
      fcIndentNodes: TObjectList;

      procedure ProcessNode(const pcNode: TObject; const pbIncrement: boolean);

    public
    constructor Create; override;
    destructor Destroy; override;

    procedure PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    procedure VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult); override;

    function FinalSummary(var psMessage: string): Boolean; override;
  end;



implementation

uses SysUtils,
  ParseTreeNode, ParseTreeNodeType, TokenType, SourceToken;

constructor TVisitSetNestings.Create;
begin
  inherited;

   fcRunningTotals := TNestingLevelList.Create;
   fcIndentNodes := TObjectList.Create;
   fcIndentNodes.OwnsObjects := False;
end;

destructor TVisitSetNestings.Destroy;
begin
  FreeAndNil(fcRunningTotals);
  FreeAndNil(fcIndentNodes);
  inherited;
end;

function TVisitSetNestings.FinalSummary(var psMessage: string): Boolean;
begin
  psMessage := fcRunningTotals.FinalTest;
  Result := (psMessage <> '');
end;

procedure TVisitSetNestings.PreVisitParseTreeNode(const pcNode: TObject; var prVisitResult: TRVisitResult);
begin
  // increment when you enter
  ProcessNode(pcNode, True);
end;

procedure TVisitSetNestings.PostVisitParseTreeNode(const pcNode: TObject);
begin
  // decrement when you exit 
  ProcessNode(pcNode, False);
end;

procedure TVisitSetNestings.ProcessNode(const pcNode: TObject; const pbIncrement: boolean);
var
  lcNode: TParseTreeNode;
  leNestType: TNestingLevelType;
  lbHasNesting: Boolean;
begin
  lbHasNesting := False;
  leNestType := nlProcedure; // must have value to supress warning

  lcNode := TParseTreeNode(pcNode);

  case lcNode.NodeType of
    nBlock, nCaseStatement, nElseCase,
    nIfBlock, nElseBlock, nTryBlock, nFinallyBlock, nExceptBlock,
    nRepeatStatement, nWhileStatement, nForStatement,
    nWithStatement, nOnExceptionHandler:
    begin
      leNestType := nlBlock;
      lbHasNesting := True;
    end;
    nCaseSelector:
    begin
      leNestType := nlCaseSelector;
      lbHasNesting := True;
    end;
    nRecordVariantSection:
    begin
      leNestType := nlRecordVariantSection;
      lbHasNesting := True;
    end;
    nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl:
    begin
      leNestType := nlProcedure;
      lbHasNesting := True;
    end;
  end;

  { test for a begin..end block with no other indent }
  if (not lbHasNesting) and (lcNode.Parent <> nil) and
    (lcNode.NodeType = nCompoundStatement) then
  begin
    if (fcIndentNodes.IndexOf(lcNode.Parent) < 0) and
      ((fcIndentNodes.IndexOf(lcNode.Parent.Parent) < 0) or (lcNode.Parent.NodeType <> nStatement)) and
      (not lcNode.HasParentNode(nElseCase, 3)) then
    begin
      leNestType := nlBlock;
      lbHasNesting := True;
    end;
  end;

  if lbHasNesting then
  begin
    if pbIncrement then
      fcRunningTotals.IncLevel(leNestType)
    else
      fcRunningTotals.DecLevel(leNestType);

    if fcIndentNodes.IndexOf(pcNode) < 0 then
      fcIndentNodes.Add(pcNode);
  end;
end;

procedure TVisitSetNestings.VisitSourceToken(const pcToken: TObject; var prVisitResult: TRVisitResult);
var
  lcToken: TSourceToken;
begin
  lcToken := TSourceToken(pcToken);

  case lcToken.TokenType of
    ttCloseBracket:
      fcRunningTotals.DecLevel(nlRoundBracket);
    ttCloseSquareBracket:
      fcRunningTotals.DecLevel(nlSquareBracket);
  end;


  // store the total so far on this leaf
  lcToken.Nestings.Assign(fcRunningTotals);

  case lcToken.TokenType of
    ttOpenBracket:
      fcRunningTotals.IncLevel(nlRoundBracket);
    ttOpenSquareBracket:
      fcRunningTotals.IncLevel(nlSquareBracket);
  end;

end;


end.
