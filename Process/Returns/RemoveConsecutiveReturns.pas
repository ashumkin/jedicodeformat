unit RemoveConsecutiveReturns;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveEmptyComment, released Nov 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
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

{ AFS 9 Nov 2003
  Remove consecutive returns
  ie put an upper limit on the number of blank lines in a row }

uses SwitchableVisitor, VisitParseTree;

type
  TRemoveConsecutiveReturns = class(TSwitchableVisitor)
  private
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
  FormatFlags, SourceToken, Tokens, TokenUtils, JcfSettings;


constructor TRemoveConsecutiveReturns.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

procedure TRemoveConsecutiveReturns.EnabledVisitSourceToken(
  const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
  liCount: integer;
begin
  lcSourceToken := TSourceToken(pcNode);

  liCount := 0;

  while (lcSourceToken <> nil) and (lcSourceToken.TokenType = ttReturn) do
  begin
    inc(liCount);

    if liCount > FormatSettings.Returns.MaxConsecutiveReturns then
    begin
      BlankToken(lcSourceToken);
    end;

    lcSourceToken := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
  end;

end;

function TRemoveConsecutiveReturns.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Returns.RemoveConsecutiveReturns;
end;

end.
