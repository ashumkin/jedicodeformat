unit MoveSpaceToBeforeColon;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter MoveSpaceToBeforeColon code

The Original Code is SingleSpaceAfter, released December 2008.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ AFS 14 December 2009
  Process to move spaces to after colon
  SF Bug request #2173842
}

uses SwitchableVisitor;

type
  TMoveSpaceToBeforeColon = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  FormatFlags, SourceToken, Tokens, TokenUtils;

constructor TMoveSpaceToBeforeColon.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace, eRemoveReturn];
end;

function TMoveSpaceToBeforeColon.EnabledVisitSourceToken(const pcNode: TObject): boolean;
var
  lcSourceToken: TSourceToken;
  lcPrev: TSourceToken;
  lcNew: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttColon then
  begin
    lcPrev := lcSourceToken.PriorToken;

    if (lcPrev <> nil) and (lcPrev.TokenType = ttWhiteSpace) then
    begin
      // put the space after
      lcNew := TSourceToken.Create;
      lcNew.TokenType := ttWhiteSpace;
      lcNew.SourceCode := lcPrev.SourceLine;

      InsertTokenAfter(lcSourceToken, lcNew);

      BlankToken(lcPrev);
    end;

  end;
end;

function TMoveSpaceToBeforeColon.IsIncludedInSettings: boolean;
begin
  Result := true;
end;

end.
