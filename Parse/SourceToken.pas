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

unit SourceToken;

{ Created AFS 29 Nov 1999
  Token  - element of source code text }

interface

uses
    { delphi } Windows,
    { local } TokenType, WordMap, ParseTreeNode, VisitParseTree;

type

  TSourceToken = class(TParseTreeNode)
  private
    { property implementation }
    fsSourceCode: string;
    feTokenType: TTokenType;
    feWord: TWord;
    feCommentStyle: TCommentStyle;

    fiXPosition, fiYPosition: integer;


  protected
  public
    constructor Create;

    function Describe: string; override;
    procedure AcceptVisitor(const pcVisitor: IVisitParseTree); override;

    function IsSolid: boolean;

    function HasChildNode(const peWords: TWordSet): Boolean; override;

    property TokenType: TTokenType read feTokenType write feTokenType;
    property SourceCode: string read fsSourceCode write fsSourceCode;
    property Word: TWord read feWord write feWord;
    property CommentStyle: TCommentStyle read feCommentStyle write feCommentStyle;

    property XPosition: integer read fiXPosition write fiXPosition;
    property YPosition: integer read fiYPosition write fiYPosition;
  end;

  TSourceTokenProcedure = procedure(const pt: TSourceToken) of object;

implementation

uses
    { delphi } Classes, SysUtils,
    { local } JclStrings;

{-------------------------------------------------------------------------------
 TSourceToken }


constructor TSourceToken.Create;
begin
  inherited;

  feTokenType  := ttUnknown;
  feWord       := wUnknown;
  fsSourceCode := '';
end;

function TSourceToken.Describe: string;
const
  StructuredTokens: TTokenTypeSet =
    [ttComment, ttOperator, ttNumber, ttLiteralString, ttUnKnown];
begin
  Result := TokenTypeToString(TokenType);
  if (TokenType in (TextualTokens + StructuredTokens)) then
    Result := Result + ' ' + SourceCode;
end;

function TSourceToken.HasChildNode(const peWords: TWordSet): Boolean;
begin
  Result := (Word in peWords);
end;

function TSourceToken.IsSolid: boolean;
begin
  Result := not (TokenType in NotSolidTokens);
end;


procedure TSourceToken.AcceptVisitor(const pcVisitor: IVisitParseTree);
begin
  Assert(pcVisitor <> nil);
  pcVisitor.VisitSourceToken(self);
end;

end.