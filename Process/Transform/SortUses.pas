unit SortUses;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SortUses.pas, released July 2004.
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

{ unit to sort the uses clause }

uses BaseVisitor, ParseTreeNode;

type
  TSortUses = class(TBaseTreeNodeVisitor)
  private

    procedure SortUsesInSections(pcUses: TParseTreeNode);

  protected
  public
    constructor Create; override;

    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { delphi }
  Contnrs, Classes, SysUtils, Math,
  { local }
  ParseTreeNodeType, SourceToken, Tokens, JcfSettings,
  SetTransform, TokenUtils;

function GetSortableToken(pcNode: TParseTreeNode): TSourceToken;
var
  lcTest: TParseTreeNode;
begin
  Result := nil;

  if pcNode is TSourceToken then
  begin
    Result := TSourceToken(pcNode);
  end
  else
  begin
    lcTest := pcNode.GetImmediateChild(nUsesItem);
    if lcTest <> nil then
      pcNode := lcTest;

    lcTest := pcNode.GetImmediateChild(nIdentifier);
    if lcTest <> nil then
      pcNode := lcTest;

    lcTest := pcNode.FirstSolidLeaf;
    if lcTest <> nil then
      Result := TSourceToken(lcTest);
  end;

end;

function AlphaNameSort(Item1, Item2: Pointer): integer;
var
  lc1, lc2: TParseTreeNode;
  lcToken1, lcToken2: TSourceToken;
begin
  lc1 := TParseTreeNode(Item1);
  lc2 := TParseTreeNode(Item2);

  lcToken1 := GetSortableToken(lc1);
  lcToken2 := GetSortableToken(lc2);

  if lcToken1 = nil then
    Result := -1
  else if lcToken2 = nil then
    Result := 1
  else
  begin
    Result := AnsiCompareText(lcToken1.SourceCode, lcToken2.SourceCode);
  end;
end;

function ReverseAlphaNameSort(Item1, Item2: Pointer): integer;
begin
  Result := AlphaNameSort(Item1, Item2) * -1;
end;

function LengthNameSort(Item1, Item2: Pointer): integer;
var
  lc1, lc2: TParseTreeNode;
  lcToken1, lcToken2: TSourceToken;
begin
  lc1 := TParseTreeNode(Item1);
  lc2 := TParseTreeNode(Item2);

  lcToken1 := GetSortableToken(lc1);
  lcToken2 := GetSortableToken(lc2);

  if lcToken1 = nil then
    Result := -1
  else if lcToken2 = nil then
    Result := 1
  else
  begin
    Result := Sign(Length(lcToken1.SourceCode) - Length(lcToken2.SourceCode));

    { Lenght has more sort collisions that alphabetic
      same length -> Alphabetic sort }
    if Result = 0 then
      Result := AnsiCompareText(lcToken1.SourceCode, lcToken2.SourceCode);
  end;
end;

function ReverseLengthNameSort(Item1, Item2: Pointer): integer;
var
  lc1, lc2: TParseTreeNode;
  lcToken1, lcToken2: TSourceToken;
begin
  lc1 := TParseTreeNode(Item1);
  lc2 := TParseTreeNode(Item2);

  lcToken1 := GetSortableToken(lc1);
  lcToken2 := GetSortableToken(lc2);

  if lcToken1 = nil then
    Result := -1
  else if lcToken2 = nil then
    Result := 1
  else
  begin
    Result := Sign(Length(lcToken2.SourceCode) - Length(lcToken1.SourceCode));

    { Soting the other way by length, but still forwards alphbetically }
    if Result = 0 then
      Result := AnsiCompareText(lcToken1.SourceCode, lcToken2.SourceCode);
  end;
end;

procedure RepunctuateUses(const pcList: TParseTreeNode);
var
  liLoop: integer;
  lcItem: TParseTreeNode;
  lcToken, lcNext, lcNew: TSourceToken;
  lbFoundLast: Boolean;
begin
  { remove existing punctuation }
  for liLoop := pcList.ChildNodeCount - 1 downto 0 do
  begin
    lcItem := pcList.ChildNodes[liLoop];
    if lcItem is TSourceToken then
    begin
      { commas will have migrated to the top }
      lcToken := TSourceToken(lcItem);
      if lcToken.TokenType = ttComma then
        pcList.RemoveChild(liLoop)

      { likewise returns }
      else if lcToken.TokenType = ttReturn then
      begin
        lcNext := lcToken.NextTokenWithExclusions([ttWhiteSpace]);
        if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
          pcList.RemoveChild(liLoop);
      end;
    end;
  end;

  { commas after all uses items but the last }
  lbFoundLast := False;
  for liLoop := pcList.ChildNodeCount - 1 downto 0 do
  begin
    lcItem := pcList.ChildNodes[liLoop];

    if (lcItem.NodeType = nUsesItem) then
    begin
      if not lbFoundLast then
      begin
        lbFoundLast := True;
      end
      else
      begin
        lcNew := TSourceToken.Create;

        lcNew.TokenType := ttComma;
        lcNew.SourceCode := ',';

        pcList.InsertChild(liLoop + 1, lcNew);
      end;
    end;
  end;

  { a space before the first one }
  lcItem := pcList.ChildNodes[0];
  lcToken := TSourceToken(lcItem.FirstLeaf);
  if (lcToken = nil) or (lcToken.TokenType <> ttWhiteSpace) then
    pcList.InsertChild(0, NewSpace(1));

  { return after all comments if BreakUsesSortOnComment
  otherwise just after // comments }
  for liLoop := pcList.ChildNodeCount - 2 downto 0 do
  begin
    lcItem := pcList.ChildNodes[liLoop];

    if lcItem is TSourceToken then
    begin
      { commas will have migrated to the top }
      lcToken := TSourceToken(lcItem);

      if lcToken.TokenType = ttComment then
      begin
        if (lcToken.CommentStyle = eDoubleSlash) or (FormatSettings.Transform.BreakUsesSortOnComment) then
        begin
          lcNext := lcToken.NextTokenWithExclusions([ttWhiteSpace]);
          if lcNext.TokenType <> ttReturn then
            pcList.InsertChild(liLoop + 1, NewReturn);
        end;
      end;
    end;
  end;

end;

constructor TSortUses.Create;
begin
  inherited;

  HasPostVisit := True;
  HasSourceTokenVisit := False;
end;

function TSortUses.IsIncludedInSettings: boolean;
begin
  with FormatSettings.Transform do
    Result := SortInterfaceUses or SortImplementationUses;
end;

procedure TSortUses.PostVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcIdentList: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  { is this a uses clause? }
  if lcNode.NodeType <> nUses then
    exit;

  { is it turned on in this section? }
  if lcNode.HasParentNode(nInterfaceSection) then
  begin
    if not FormatSettings.Transform.SortInterfaceUses then
      exit;
  end
  else if lcNode.HasParentNode(nImplementationSection) then
  begin
    if not FormatSettings.Transform.SortImplementationUses then
      exit;
  end
  else
    exit;

  lcIdentList := lcNode.GetImmediateChild(nIdentList);
  if lcIdentList <> nil then
  begin
    SortUsesInSections(lcIdentList);
    RepunctuateUses(lcIdentList);
  end;
end;

procedure TSortUses.SortUsesInSections(pcUses: TParseTreeNode);
var
  leBreakTokens, leExcludeTokens: TTokenTypeSet;
  lcSections: TObjectList;
  lcCurrentSection: TObjectList;
  liLoop: integer;
  lcItem: TParseTreeNode;
  lcToken: TSourceToken;

  liSectionLoop, liItemLoop: integer;
begin
  leBreakTokens := [];
  leExcludeTokens := [ttWhiteSpace];

  if FormatSettings.Transform.BreakUsesSortOnReturn then
    Include(leBreakTokens, ttReturn)
  else
    Include(leExcludeTokens, ttReturn);

  if FormatSettings.Transform.BreakUsesSortOnComment then
    Include(leBreakTokens, ttComment)
  else
    Include(leExcludeTokens, ttComment);

  { sections is a list of lists
    each section is a list of parse tree nodes
    Each section is sorted }
  lcSections := TObjectList.Create;
  lcSections.OwnsObjects := True;
  
  try
    { at least one section }
    lcCurrentSection := TObjectList.Create;
    lcCurrentSection.OwnsObjects := False;
    
    lcSections.Add(lcCurrentSection);

    { each child node is in a section }
    for liLoop := 0 to pcUses.ChildNodeCount - 1 do
    begin
      lcItem := pcUses.ChildNodes[liLoop];

      lcToken := TSourceToken(lcItem.FirstLeaf);
      if lcToken.TokenType in leExcludeTokens then
        lcToken := lcToken.NextTokenWithExclusions(leExcludeTokens);

      { end of section, start of the next  }
      if (lcToken.TokenType in leBreakTokens) and
        (liLoop < (pcUses.ChildNodeCount - 1 )) and
        (liLoop > 0) then
      begin
        lcCurrentSection := TObjectList.Create;
        lcCurrentSection.OwnsObjects := False;
        lcSections.Add(lcCurrentSection);
      end;

      lcCurrentSection.Add(lcItem);
    end;

    { remove original entries, but don't free them }
    while pcUses.ChildNodeCount > 0 do
      pcUses.ExtractChild(pcUses.ChildNodes[0]);

    { sort each section }
    for liSectionLoop := 0 to lcSections.Count - 1 do
    begin
      lcCurrentSection := TObjectList(lcSections.Items[liSectionLoop]);

      { sort this section }
      case FormatSettings.Transform.UsesSortOrder of
        eAlpha:
          lcCurrentSection.Sort(AlphaNameSort);
        eReverseAlpha:
          lcCurrentSection.Sort(ReverseAlphaNameSort);
        eShortToLong:
          lcCurrentSection.Sort(LengthNameSort);
        eLongToShort:
          lcCurrentSection.Sort(ReverseLengthNameSort);
        else
          Assert(False);
      end;

      {  repopulate the original parent with sorted items }
      for liItemLoop := 0 to lcCurrentSection.Count - 1 do
      begin
        lcItem := TParseTreeNode(lcCurrentSection.Items[liItemLoop]);
        pcUses.AddChild(lcItem);
      end;

    end;

  finally
    lcSections.Free;
  end;

end;

end.
