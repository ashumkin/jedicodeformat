unit SpaceToTab;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SpaceToTab, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
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

{ AFS 4 Jan 2002
  convert spaces tabs }

uses SwitchableVisitor, VisitParseTree;


type
  TSpaceToTab = class(TSwitchableVisitor)
  private
    fsSpaces: string;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject;
      var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, SourceToken, Tokens, FormatFlags;

constructor TSpaceToTab.Create;
begin
  inherited;
  fsSpaces    := StrRepeat(AnsiSpace, FormatSettings.Spaces.SpacesForTab);
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

procedure TSpaceToTab.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls, lsTab:     string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if not (lcSourceToken.TokenType in [ttWhiteSpace, ttComment]) then
    exit;

  { can't pass property as var parameter so ls local var is used }
  ls    := lcSourceToken.SourceCode;
  lsTab := AnsiTab;
  StrReplace(ls, fsSpaces, lsTab, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

function TSpaceToTab.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.SpacesToTabs;
end;

end.
