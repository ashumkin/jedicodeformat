{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is AlignStatements.pas, released April 2000.
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
unit AlignBase;


{ AFS 6 Feb 2K
  base class Generalisation of AlignAssign and co.
  This is the base class for all aligners
}

interface

uses SwitchableVisitor, VisitParseTree, SourceToken, SourceTokenList;

type

  TAlignBase = class(TSwitchableVisitor)
  private
    fcTokens: TSourceTokenList;
    fcResumeToken: TSourceToken;

    procedure AlignTheBlock(const pcToken: TSourceToken);
    procedure IndentAll(piTokens, piIndent: integer);

    function StillSuspended(const pc: TSourceToken): boolean;

  protected
    fbKeepComments: Boolean;


    { API for descendant classes }
    function TokenIsAligned(const pt: TSourceToken): boolean; virtual; abstract;
    function TokenEndsStatement(const pt: TSourceToken): boolean; virtual; abstract;
    function IsTokenInContext(const pt: TSourceToken): boolean; virtual;


      { override this to let the child class see the tokens as they come
      this is used by the align vars to detect the first non-white space token after the : }

    procedure OnTokenRead(const pt: TSourceToken); virtual;
    procedure ResetState; virtual;

    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
    destructor Destroy; override;
end;

implementation

uses
  { delphi } SysUtils, Math,
  { jcl } JclStrings,
  { jcf } TokenType, JcfSettings, TokenUtils;

{ TAlignBase }

constructor TAlignBase.Create;
begin
  inherited;
  fbKeepComments := False;
  fcResumeToken := nil;
  fcTokens := TSourceTokenList.Create;
end;


destructor TAlignBase.Destroy;
begin
  FreeAndNil(fcTokens);
  inherited;
end;


procedure TAlignBase.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  lcToken, lcNext: TSourceToken;
begin
  lcToken := TSourceToken(pcNode);

  lcNext := lcToken.NextToken;
  if lcNext = nil then
    exit;

  if not IsTokenInContext(lcNext) then
    exit;

  if StillSuspended(lcNext) then
    exit;

  if TokenIsAligned(lcNext) then
    AlignTheBlock(lcNext);
end;


procedure TAlignBase.AlignTheBlock(const pcToken: TSourceToken);
var
  liCurrent, liLastKnownAlignedStatement: integer;
  lcCurrent: TSourceToken;
  bDone, bThisStatementIsAligned: boolean;
  liMaxIndent, liMinIndent: integer;
  liThisIndent: integer;
  liSettingsMin, liSettingsMax, liSettingsMaxVariance: integer;

  liUnalignedCount, liMaxUnaligned: integer;
  liAlignedCount: integer;
begin
  ResetState;

  Assert(TokenIsAligned(pcToken));

  lcCurrent := pcToken;
  fcTokens.Add(lcCurrent);
  OnTokenRead(lcCurrent);

  liLastKnownAlignedStatement := 0;
  liMaxIndent := lcCurrent.XPosition;
  liMinIndent := liMaxIndent;

  { locate end of first statement
   BufferTokens(0) is the first :=
   there must be a semicolon soon after }

  liCurrent := 0;
  repeat
    inc(liCurrent);
    lcCurrent := lcCurrent.NextToken;
    fcTokens.Add(lcCurrent);
    OnTokenRead(lcCurrent);
  until (lcCurrent = nil) or TokenEndsStatement(lcCurrent);

  inc(liCurrent); { liCurrent is the index of a statement ender so move past it }

  { end the first statement on EOF?! - abort! }
  if (lcCurrent = nil) or (lcCurrent.TokenType = ttEOF) then
    exit;

  with Settings do
  begin
    liSettingsMin := Align.MinColumn;
    liSettingsMax := Align.MaxColumn;
    liSettingsMaxVariance := Align.MaxVariance;
    liMaxUnaligned := Align.MaxUnalignedStatements;
  end;

  { locate block end - include all consecutive aligned statements }
  bDone := False;
  liUnalignedCount := 0;
  liAlignedCount := 1;
  bThisStatementIsAligned := True; // first statement just read will be aligned


  { jow look for consecutive similar statements to align }
  while not bDone do
  begin
    lcCurrent := lcCurrent.NextToken;

    { EOF?! - abort! }
    if (lcCurrent = nil) or (lcCurrent.TokenType = ttEOF) then
    begin
      bDone := True;
    end
    else
    begin
      fcTokens.Add(lcCurrent);
      OnTokenRead(lcCurrent);

      { an aligned statement has the aligned token in it -
        e.g. an assign statement has a ':=' in it :) }
      if TokenIsAligned(lcCurrent) then
      begin
        bThisStatementIsAligned := True;

        liThisIndent := lcCurrent.XPosition;

        if liThisIndent >= liSettingsMin then
          liMinIndent := Min(liThisIndent, liMinIndent);

        { store the higest indent in liMaxIndent
          unless it is out of bounds, ie < liSettingsMin or > liSettingsMax }
        liThisIndent := Max(liThisIndent, liSettingsMin);
        if (liThisIndent > liMaxIndent) and (liThisIndent < liSettingsMax) and
          (liThisIndent <= liMinIndent + liSettingsMaxVariance) then
          liMaxIndent := liThisIndent;

        { may need to knock down the min if the first one is an outlier }
        if (liThisIndent + liSettingsMaxVariance) < liMaxIndent then
          liMaxIndent := liThisIndent;

      end;
      { carry on through comments - not valid anymore ??
        Why didn't I explain why this was in?
        May be of use on some of the aligners so it has been make a setting

        Use only where the comment is first solid token on line ?
         (ie not of enline comments)
      }
      if fbKeepComments and (lcCurrent.TokenType = ttComment) then
        bThisStatementIsAligned := True;

      if TokenEndsStatement(lcCurrent) then
      begin
        { ending a statement - was it an aligned one?
          If not, maybe we should have stopped with the last statement }

        if bThisStatementIsAligned then
        begin
          liLastKnownAlignedStatement := liCurrent;
          liUnalignedCount := 0;
          bThisStatementIsAligned := False;
          inc(liAlignedCount);
        end
        else
        begin
          { look for consecutive unaligned statements to end the aligned block
            depending on the config, this could be just 1 unalaigned statement
            or it could be more
          }
          inc(liUnalignedCount);
          if liUnalignedCount > liMaxUnaligned then
            bDone := True;
        end;
      end;

      inc(liCurrent);
    end; { not EOF }
  end; { while loop }

  { set iResume equal to the last token aligned  }
  fcResumeToken := fcTokens.SourceTokens[fcTokens.Count - 1];

  { now we know how far to go and how far to indent, do it }
  if (liLastKnownAlignedStatement > 0) and (liAlignedCount > 1) then
    IndentAll(liLastKnownAlignedStatement, liMaxIndent);

  ResetState;
end;

procedure TAlignBase.IndentAll(piTokens, piIndent: integer);
var
  liLoop: integer;
  lcCurrent, lcNew: TSourceToken;
begin
  liLoop := 0;
  while liLoop <= piTokens do
  begin
    lcCurrent := fcTokens.SourceTokens[liLoop];
    OnTokenRead(lcCurrent);

    if TokenIsAligned(lcCurrent) and (lcCurrent.XPosition < piIndent) then
    begin
      { indent to the specified level  - make a new space token }
      lcNew := InsertSpacesBefore(lcCurrent, piIndent - lcCurrent.XPosition);

      { list just got longer - move the end marker }
      fcTokens.Insert(liLoop, lcNew);
      inc(piTokens);
      inc(liLoop);
    end;

    inc(liLoop);
  end;
end;



procedure TAlignBase.OnTokenRead(const pt: TSourceToken);
begin
  // here for override
end;

procedure TAlignBase.ResetState;
begin
  fcTokens.Clear;
end;

function TAlignBase.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  // here for override
  Result := True;
end;

function TAlignBase.StillSuspended(const pc: TSourceToken): boolean;
begin
  if (fcResumeToken = nil) then
  begin
    // we are not suspended, so go.
    Result := False;
  end
  else
  begin
    // have we reached the end of suspension?
    Result := (fcResumeToken <> pc);
    if not Result then
      fcResumeToken := nil;
  end;

end;

end.
