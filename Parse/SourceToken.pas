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
    { local } Tokens, ParseTreeNode, VisitParseTree;

type

  TSourceToken = class(TParseTreeNode)
  private
    { property implementation }
    fsSourceCode: string;
    feTokenType: TTokenType;
    feWordType: TWordType;
    feCommentStyle: TCommentStyle;

    fiXPosition, fiYPosition: integer;
    fiSolidTokenOnLineIndex: integer;

  protected
  public
    constructor Create;

    function Describe: string; override;
    function DescribePosition: string;

    procedure AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult); override;

    function IsSolid: boolean;

    function HasChildNode(const peTokens: TTokenTypeSet): Boolean; override;
    function HasChildNode(const peTokens: TTokenTypeSet; const piMaxDepth: integer): Boolean; override;

    function SolidChildCount: integer; override;
    function FirstSolidLeaf: TParseTreeNode; override;
    function IsLeaf: Boolean; override;

    { navigating the source tree as if it was a list }
    function NextToken: TSourceToken;
    function PriorToken: TSourceToken;
    function NextSolidToken: TSourceToken;
    function PriorSolidToken: TSourceToken;
    function NextTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;
    function PriorTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;

    function IsHashLiteral: boolean;

    property TokenType: TTokenType read feTokenType write feTokenType;
    property WordType: TWordType read feWordType write feWordType;

    property SourceCode: string read fsSourceCode write fsSourceCode;
    property CommentStyle: TCommentStyle read feCommentStyle write feCommentStyle;

    property XPosition: integer read fiXPosition write fiXPosition;
    property YPosition: integer read fiYPosition write fiYPosition;
    property SolidTokenOnLineIndex: integer read fiSolidTokenOnLineIndex write fiSolidTokenOnLineIndex;
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
  fsSourceCode := '';
end;



function TSourceToken.Describe: string;
const
  StructuredTokens: TTokenTypeSet =
    [ttComment, ttNumber, ttLiteralString, ttUnknown, ttPunctuation, ttIdentifier];
begin
  if TokenType = ttIdentifier then
    Result := SourceCode
  else
  begin
    Result := TokenTypeToString(TokenType);
    if (TokenType in StructuredTokens) then
      Result := Result + ' ' + SourceCode;
  end;
end;

function TSourceToken.DescribePosition: string;
begin
  Result := '';

  if YPosition > 0 then
  begin
    Result := Result + 'on line ' + IntToStr(YPosition);

    if XPosition > 0 then
      Result := Result + ' position ' + IntToStr(XPosition);
  end;
end;

function TSourceToken.HasChildNode(const peTokens: TTokenTypeSet): Boolean;
begin
  Result := (TokenType in peTokens);
end;

function TSourceToken.HasChildNode(const peTokens: TTokenTypeSet; const piMaxDepth: integer): Boolean;
begin
  Result := (TokenType in peTokens);
end;

function TSourceToken.IsSolid: boolean;
begin
  Result := not (TokenType in NotSolidTokens);
end;


procedure TSourceToken.AcceptVisitor(const pcVisitor: IVisitParseTree; var prVisitResults: TRVisitResult);
begin
  Assert(pcVisitor <> nil);
  pcVisitor.VisitSourceToken(self, prVisitResults);
end;


function TSourceToken.NextToken: TSourceToken;
var
  lcTemp: TParseTreeNode;
begin
  Result := nil;
  lcTemp := NextLeafNode;
  if lcTemp = nil then
    exit;

  Assert(lcTemp is TSourceToken, 'Next leaf is not token at ' + Describe);
  Result := TSourceToken(lcTemp);
end;

function TSourceToken.PriorToken: TSourceToken;
var
  lcTemp: TParseTreeNode;
begin
  Result := nil;
  lcTemp := PriorLeafNode;
  if lcTemp = nil then
    exit;

  Assert(lcTemp is TSourceToken, 'prior leaf is not token at ' + Describe);
  Result := TSourceToken(lcTemp);
end;

function TSourceToken.NextSolidToken: TSourceToken;
begin
  Result := NextToken;

  while (Result <> nil) and (not Result.IsSolid) do
    Result := Result.NextToken;
end;

function TSourceToken.NextTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;
begin
  Result := NextToken;

  while (Result <> nil) and (Result.TokenType in peExclusions) do
    Result := Result.NextToken;
end;

function TSourceToken.PriorTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;
begin
  Result := PriorToken;

  while (Result <> nil) and (Result.TokenType in peExclusions) do
    Result := Result.PriorToken;
end;


function TSourceToken.PriorSolidToken: TSourceToken;
begin
  Result := PriorToken;

  while (Result <> nil) and (not Result.IsSolid) do
    Result := Result.PriorToken;
end;

function TSourceToken.SolidChildCount: integer;
begin
  if IsSolid then
    Result := 1
  else
    Result := 0;
end;

function TSourceToken.FirstSolidLeaf: TParseTreeNode;
begin
  if IsSolid then
    Result := self
  else
    Result := nil;
end;

function TSourceToken.IsHashLiteral: boolean;
begin
  Result := (TokenType = ttLiteralString) and (StrLeft(SourceCode, 1) = '#');
end;

function TSourceToken.IsLeaf: Boolean;
begin
  Result := True;
end;

end.
