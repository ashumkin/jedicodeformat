unit MaxSpaces;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is MaxSpaces, released January 2004.
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

{ AFS 4 Jan 2002
  convert spaces tabs }

uses SwitchableVisitor, VisitParseTree;


type
  TMaxSpaces = class(TSwitchableVisitor)
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
  JcfSettings, SourceToken, Tokens, FormatFlags, SetSpaces;

constructor TMaxSpaces.Create;
begin
  inherited;
  fsSpaces := StrRepeat(AnsiSpace, FormatSettings.Spaces.MaxSpacesInCode);
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

procedure TMaxSpaces.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { don't truncate the indentation spaces }
  if lcSourceToken.SolidTokenOnLineIndex > 0 then
  begin
    { if the token is too long, truncate it }
    if Length(lcSourceToken.SourceCode) > FormatSettings.Spaces.MaxSpacesInCode then
    begin
      lcSourceToken.SourceCode := fsSpaces;
    end;
  end;
end;

function TMaxSpaces.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.UseMaxSpacesInCode;
end;

end.
