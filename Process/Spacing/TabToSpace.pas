unit TabToSpace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TabToSpace, released May 2003.
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
  convert tabs to spaces }

uses SwitchableVisitor, VisitParseTree;


type
  TTabToSpace = class(TSwitchableVisitor)
  private
    fsSpaces: string;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  SysUtils,
  JclStrings,
  JcfSettings, SourceToken, Tokens, FormatFlags;

constructor TTabToSpace.Create;
begin
  inherited;
  fsSpaces := StrRepeat(AnsiSpace, FormatSettings.Spaces.SpacesPerTab);
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

procedure TTabToSpace.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  ls: string;
begin
  lcSourceToken := TSourceToken(pcNode);

  if not (lcSourceToken.TokenType in [ttWhiteSpace, ttComment]) then
    exit;

  { can't pass property as var parameter so ls local var is used }
  ls := lcSourceToken.SourceCode;
  StrReplace(ls, AnsiTab, fsSpaces, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

function TTabToSpace.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.TabsToSpaces;
end;

end.