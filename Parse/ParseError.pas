unit ParseError;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ParseError, released May 2003.
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

uses
  {delphi }
  SysUtils,
  { local }
  SourceToken;

type
  TEParseError = class(Exception)
  private
    fcToken: TSourceToken;
    function GetTokenMessage: string;

  public
    Constructor Create(const psMessage: string; const pcToken: TSourceToken);

    property TokenMessage: string read GetTokenMessage;
  end;

implementation

{ TEParseError }

constructor TEParseError.Create(const psMessage: string; const pcToken: TSourceToken);
begin
  inherited Create(psMessage);

  fcToken := pcToken;
end;

function TEParseError.GetTokenMessage: string;
var
  lsPos: string;
begin
  if fcToken = nil then
    Result := ''
  else
  begin
    Result := fcToken.Describe;
    lsPos := fcToken.DescribePosition;
    if lsPos <> '' then
      Result := Result + ' ' + lsPos;
  end;

end;

end.