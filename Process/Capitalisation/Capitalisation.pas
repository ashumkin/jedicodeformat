unit Capitalisation;

{ AFS 30 December 2002
  visitor to do capitalisation according to settings
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Capitalisation, released May 2003.
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

uses SwitchableVisitor;

type
  TCapitalisation = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  SysUtils,
  JclStrings,
  SourceToken, SettingsTypes, Tokens, ParseTreeNodeType,
  JcfSettings, FormatFlags;

{ TCapitalisation }

procedure FixCaps(const pct: TSourceToken; const caps: TCapitalisationType);
begin
  if pct = nil then
    exit;
  if pct.SourceCode = '' then
    exit;

  case caps of
    ctUpper:
      pct.SourceCode := AnsiUpperCase(pct.SourceCode);
    ctLower:
      pct.SourceCode := AnsiLowerCase(pct.SourceCode);
    ctMixed:
      pct.SourceCode := StrSmartCase(pct.SourceCode, []);
    ctLeaveAlone: ;
  end;
end;

function TCapitalisation.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Caps.Enabled;
end;

constructor TCapitalisation.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eCapsReservedWord];
end;

function TCapitalisation.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  case lcSourceToken.WordType of
    wtReservedWord:
      FixCaps(lcSourceToken, FormatSettings.Caps.ReservedWords);
    wtReservedWordDirective:
    begin
      { directives can occur in other contexts - they are valid proc & variable names
        so we need to know if this one was parsed as a directive }
      if lcSourceToken.HasParentNode(DirectiveNodes) then
        FixCaps(lcSourceToken, FormatSettings.Caps.Directives);
    end;
    wtBuiltInConstant:
      FixCaps(lcSourceToken, FormatSettings.Caps.Constants);
    wtOperator:
      FixCaps(lcSourceToken, FormatSettings.Caps.Operators);
    wtBuiltInType:
      FixCaps(lcSourceToken, FormatSettings.Caps.Types);
  end;
end;

end.
