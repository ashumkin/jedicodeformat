unit FindReplace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is FindReplace.pas, released April 2000.
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

uses SwitchableVisitor, VisitParseTree;


type
  TFindReplace = class(TSwitchableVisitor)
  private
    fiCount: integer;

  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    { return true if you want the message logged}
    function FinalSummary(var psMessage: string): Boolean; override;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfSettings,
  SourceToken,
  FormatFlags;

{ TFindReplace }

constructor TFindReplace.Create;
begin
  inherited;
  fiCount := 0;

  FormatFlags := FormatFlags + [eFindReplace];
end;

function TFindReplace.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Replace.Enabled;
end;

function TFindReplace.FinalSummary(var psMessage: string): Boolean;
begin
  Result := (fiCount > 0);
  if Result then
    psMessage := 'Replace: ' + IntToStr(fiCount) + ' changes were made';
end;


procedure TFindReplace.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  if pcNode = nil then
    exit;

  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.SourceCode = '' then
    exit;

  if not FormatSettings.Replace.HasWord(lcSourceToken.SourceCode) then
    exit;

  lcSourceToken.SourceCode := FormatSettings.Replace.Replace(lcSourceToken.SourceCode);
  inc(fiCount);
end;

end.
