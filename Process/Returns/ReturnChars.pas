unit ReturnChars;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReturnChars, released May 2003.
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

uses SourceToken, SwitchableVisitor, VisitParseTree;


type
  TReturnChars = class(TSwitchableVisitor)
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { jcl }
  JclStrings,
  { local }
  Tokens, SettingsTypes,
  JcfSettings;

{ TReturnChars }

constructor TReturnChars.Create;
begin
  inherited;

end;

procedure TReturnChars.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if  (lcSourceToken.TokenType <> ttReturn) then
    exit;

  case FormatSettings.Returns.ReturnChars of
    rcLeaveAsIs:
    begin
     // leave as is
    end;
    rcLinefeed:
    begin
      // easy case - CrLf with Lf
      lcSourceToken.SourceCode := AnsiLineFeed;
    end;
    rcCrLf:
    begin
      lcSourceToken.SourceCode := AnsiCrLf;
    end;
    rcPlatform:
    begin
      // AnsiLineBreak is set to the right value at compile time
      lcSourceToken.SourceCode := AnsiLineBreak;
    end;

  end;
end;

function TReturnChars.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.Returns.ReturnChars <> rcLeaveAsIs);
end;

end.