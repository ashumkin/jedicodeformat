{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Token.pas, released April 2000.
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

unit Token;

{ Created AFS 29 Nov 1999
  Token  - element of source code text }

interface

uses
    { delphi } Windows,
    { local } TokenType, WordMap, ParseTreeNode;

type

  TToken = class(TParseTreeNode)
  private
    { property implementation }
    fsSourceCode: string;
    feTokenType: TTokenType;
    feWord: TWord;


  protected
  public
    constructor Create;


    function Describe: string; override;
  end;

  TTokenProcedure = procedure(const pt: TToken) of object;

implementation

uses
    { delphi } Classes, SysUtils,
    { local } JclStrings;


function NewToken: TToken;
begin
  Result := TToken.Create;
end;


{-------------------------------------------------------------------------------
 TToken }


constructor TToken.Create;
begin
  inherited;

  feTokenType  := ttUnknown;
  feWord       := wUnknown;
  fsSourceCode := '';
end;

function TToken.Describe: string;
const
  StructuredTokens: TTokenTypeSet =
    [ttComment, ttOperator, ttNumber, ttLiteralString, ttUnKnown];
begin
  Result := TokenTypeToString(TokenType);
  if (TokenType in (TextualTokens + StructuredTokens)) then
    Result := Result + ' ' + SourceCode;
end;

end.