unit SourceTokenList;

{ AFS 24 Dec 2002
  A list of source tokens
  This is needed after the text has been turned into toekens
  until it is turned into a parse tree
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SourceTokenList, released May 2003.
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
  { delphi } Contnrs,
  { local } SourceToken, TokenType, WordMap;

type
  TSourceTokenList = class(TObject)

  private
    fcList: TObjectList;
    function GetSourceToken(const piIndex: integer): TSourceToken;

  public

    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    procedure Clear;
    
    procedure Add(const pcToken: TSourceToken);
    procedure SetXYPositions;

    function First: TSourceToken;
    function FirstTokenType: TTokenType;
    function FirstTokenWord: TWord;

    function FirstSolidToken: TSourceToken;
    function FirstSolidTokenType: TTokenType;
    function FirstSolidTokenWord: TWord;

    function SolidToken(piIndex: integer): TSourceToken;
    function SolidTokenType(piIndex: integer): TTokenType;
    function SolidTokenWord(piIndex: integer): TWord;

    function IndexOf(const pcToken: TSourceToken): integer;
    procedure Insert(const piPos: integer; const pcToken: TSourceToken);

    function ExtractFirst: TSourceToken;

    property SourceTokens[const piIndex: integer]: TSourceToken read GetSourceToken;
end;

implementation

uses
  { delphi } SysUtils,
  { local } JcfMiscFunctions;

constructor TSourceTokenList.Create;
begin
  fcList := TObjectList.Create;

  // all will be owned by a tree later
  fcList.OwnsObjects := False;
end;

destructor TSourceTokenList.Destroy;
begin
  FreeAndNil(fcList);

  inherited;
end;

function TSourceTokenList.Count: integer;
begin
  Result := fcList.Count;
end;

procedure TSourceTokenList.Clear;
begin
  fcList.Clear;
end;

function TSourceTokenList.GetSourceToken(const piIndex: integer): TSourceToken;
begin
  Result := fcList.Items[piIndex] as TSourceToken;
end;

function TSourceTokenList.First: TSourceToken;
begin
  Result := SourceTokens[0];
end;

function TSourceTokenList.FirstTokenType: TTokenType;
begin
  if Count = 0 then
    Result := ttEOF
  else
    Result := First.TokenType;
end;

function TSourceTokenList.FirstTokenWord: TWord;
begin
  if Count = 0 then
    Result := wUnknown
  else
    Result := First.Word;
end;


function TSourceTokenList.FirstSolidTokenType: TTokenType;
var
  lc: TSourceToken;
begin
  lc := FirstSolidToken;
  if lc = nil then
    Result := ttEOF
  else
    Result := lc.TokenType;
end;

function TSourceTokenList.FirstSolidTokenWord: TWord;
var
  lc: TSourceToken;
begin
  lc := FirstSolidToken;
  if lc = nil then
    Result := wUnknown
  else
    Result := lc.Word;
end;


procedure TSourceTokenList.Add(const pcToken: TSourceToken);
begin
  fcList.Add(pcToken);
end;

function TSourceTokenList.FirstSolidToken: TSourceToken;
begin
  Result := SolidToken(1);
end;

function TSourceTokenList.SolidToken(piIndex: integer): TSourceToken;
var
  liLoop: integer;
begin
  Assert(piIndex > 0);
  Result := nil;

  for liLoop := 0 to Count - 1 do
  begin
    if SourceTokens[liLoop].IsSolid then
    begin
      // found a solid token

      if piIndex > 1 then
      begin
        // go further
        dec(piIndex);
      end
      else
      begin
        // found it
        Result := SourceTokens[liLoop];
        break;
      end;
    end;
  end;
end;


function TSourceTokenList.SolidTokenType(piIndex: integer): TTokenType;
var
  lc: TSourceToken;
begin
  lc := SolidToken(piIndex);

  if lc = nil then
    Result := ttEOF
  else
    Result := lc.TokenType;
end;

function TSourceTokenList.SolidTokenWord(piIndex: integer): TWord;
var
  lc: TSourceToken;
begin
  lc := SolidToken(piIndex);

  if lc = nil then
    Result := wUnknown
  else
    Result := lc.Word;
end;

function TSourceTokenList.IndexOf(const pcToken: TSourceToken): integer;
begin
  Result := fcList.IndexOf(pcToken);
end;

function TSourceTokenList.ExtractFirst: TSourceToken;
begin
  Result := SourceTokens[0];
  fcList.Extract(Result);
end;


procedure TSourceTokenList.SetXYPositions;
var
  liLoop: integer;
  liX, liY: integer;
  lcToken: TSourceToken;
begin
  liX := 1;
  liY := 1;

  for liLoop := 0 to count - 1 do
  begin
    lcToken := SourceTokens[liLoop];
    lcToken.XPosition := liX;
    lcToken.YPosition := liY;

    AdvanceTextPos(lcToken.SourceCode, liX, liY);
  end;
end;


procedure TSourceTokenList.Insert(const piPos: integer; const pcToken: TSourceToken);
begin
  fcList.Insert(piPos, pcToken);
end;

end.