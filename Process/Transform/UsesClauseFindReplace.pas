unit UsesClauseFindReplace;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is UsesClauseFindReplace.pas, released October 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2003 Anthony Steele.
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

{ AFS 4 October 2003

  - massage the uses clause. Replace units

 }

uses
  { delphi }
  Classes,
  { local }
  SourceToken,
  SwitchableVisitor, VisitParseTree;

type
  TUsesClauseFindReplace = class(TSwitchableVisitor)
  private
    fiCount: integer;
    fbHasFind: boolean;

    function MatchesSearch(const ps: string): Boolean;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    function FinalSummary(var psMessage: string): Boolean; override;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfSettings,
  Tokens,
  FormatFlags,
  ParseTreeNodeType,
  TokenUtils;

constructor TUsesClauseFindReplace.Create;
begin
  inherited;

  FormatFlags := FormatFlags + [eFindReplaceUses];

  fiCount := 0;
end;

function TUsesClauseFindReplace.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.UsesClause.InsertInterfaceEnabled or
    FormatSettings.UsesClause.InsertImplementationEnabled);
end;

procedure TUsesClauseFindReplace.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken, lcSepAfter, lcSepBefore: TSourceToken;
  lbInterface, lbImplementation: Boolean;
begin
  if pcNode = nil then
    exit;

  lcSourceToken := TSourceToken(pcNode);

  { only do this in a uses clause }
  if not lcSourceToken.HasParentNode(nUses) then
    exit;

  lbInterface := lcSourceToken.HasParentNode(nInterfaceSection);
  if lbInterface then
    lbImplementation := False
  else
    lbImplementation := lcSourceToken.HasParentNode(nImplementationSection);

  if not (lbImplementation or lbInterface) then
    exit;

  { only proceed on one of the specified words }
  if not (lcSourceToken.TokenType = ttIdentifier) then
    exit;

  if not MatchesSearch(lcSourceToken.SourceCode) then
    exit;

  if not fbHasFind then
  begin
    { first instance, convert the name }
    fbHasFind := True;
    lcSourceToken.SourceCode := FormatSettings.UsesClause.GetReplace;
    inc(fiCount);
  end
  else
  begin
    { throw away the word and the trailing comma, as in uses clause remove  }
    BlankToken(lcSourceToken);

    lcSepAfter := lcSourceToken.NextSolidToken;

    { now if this was the last item we have a surplus comma }
    if (lcSepAfter.TokenType = ttComma) then
    begin
      BlankToken(lcSepAfter);
    end
    else if (lcSepAfter.TokenType = ttSemiColon) then
    begin
      { remove the comma before instead }
      lcSepBefore := lcSourceToken.PriorSolidToken;
      if lcSepBefore.TokenType = ttComma then
      begin
        BlankToken(lcSepBefore);
      end;

    end;
  end;
end;

function TUsesClauseFindReplace.FinalSummary(var psMessage: string): Boolean;
begin
  Result := (fiCount > 0);
  if Result then
    psMessage := 'Uses clause find/replace: ' + IntToStr(fiCount) + ' changes were made';
end;

function TUsesClauseFindReplace.MatchesSearch(const ps: string): Boolean;
begin
  Result := FormatSettings.UsesClause.Find.IndexOf(ps) >= 0;
end;

end.
