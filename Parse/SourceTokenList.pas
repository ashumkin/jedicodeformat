Unit SourceTokenList;

{ AFS 24 Dec 2002
  A list of source tokens
  This is needed after the text has been turned into tokens
  until it is turned into a parse tree

  The way that this class works has a big impact on JCF run speed
  SO it is implemented in a more low-level way to most
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SourceTokenList, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele, Adem Baba

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.
------------------------------------------------------------------------------*)
{*)}

Interface

Uses
  { delphi }
  Contnrs,
  { local }
  SourceToken,
  Tokens;

Type
  { inherit not encapsulate, for speed }
  TSourceTokenList = Class(TObjectList)
  Private
    { This is to keep an index of the next non-nil item
      when reading items out of the list
      List.Extract is a bit slow }
    FStackIndex: Integer;

    Function GetItem(const piIndex: Integer): TSourceToken;
    Procedure SetItem(const piIndex: Integer; const pcObject: TSourceToken);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Clear; Override;

    Function Add(const pcToken: TSourceToken): Integer;
    Procedure SetXYPositions;
    Procedure Insert(const piIndex: Integer; const pcItem: TSourceToken);
    Function Extract(const piIndex: Integer): TSourceToken; {CHANGED. This is now relative to StackIndex}
    Procedure Delete(const piIndex: Integer);
    Function First: TSourceToken; {CHANGED. This is now relative to StackIndex}
    Function FirstTokenType: TTokenType; {CHANGED. This is now relative to StackIndex}
    Function FirstWordType: TWordType; {CHANGED. This is now relative to StackIndex}
    Function FirstSolidToken: TSourceToken; {CHANGED. This is now relative to StackIndex}
    Function FirstSolidTokenType: TTokenType; {CHANGED. This is now relative to StackIndex}
    Function FirstSolidWordType: TWordType; {CHANGED. This is now relative to StackIndex}
    Function FirstTokenWithExclusion(Const AExclusions: TTokenTypeSet): TSourceToken; {CHANGED. This is now relative to StackIndex}
    Function SolidToken(piIndex: integer): TSourceToken; {CHANGED. This is now relative to StackIndex}
    Function SolidTokenType(const piIndex: integer): TTokenType; {CHANGED. This is now relative to StackIndex}
    Function SolidWordType(const piIndex: integer): TWordType; {CHANGED. This is now relative to StackIndex}

    Property Items[const piIndex: Integer]: TSourceToken Read GetItem Write SetItem; Default; {This is ABSOLUTE. Not relative to StackIndex}
    Property SourceTokens[const piIndex: Integer]: TSourceToken Read GetItem Write SetItem;

    Property StackIndex: Integer Read FStackIndex; {This is to keep an index of the next non-nil item}
  End;

Implementation

Uses
  { delphi } SysUtils,
  { local } JcfMiscFunctions;

Constructor TSourceTokenList.Create;
Begin
  FStackIndex := 0;
  OwnsObjects := True;
  Inherited Create(False);
End;

Destructor TSourceTokenList.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TSourceTokenList.Clear;
Begin
  Inherited Clear;
  FStackIndex := 0;
End;

Function TSourceTokenList.GetItem(const piIndex: Integer): TSourceToken;
Begin
  Result := TSourceToken(List^[piIndex]);
End;

Procedure TSourceTokenList.SetItem(const piIndex: Integer; const pcObject: TSourceToken);
Begin
  Inherited SetItem(piIndex, pcObject);
End;

Function TSourceTokenList.First: TSourceToken;
Begin
  Result := TSourceToken(List^[FStackIndex]);
End;

Function TSourceTokenList.FirstTokenType: TTokenType;
Begin
  Result := ttUnknown;
  If Count > 0
    Then Result := TSourceToken(Inherited GetItem(FStackIndex)).TokenType;
End;

Function TSourceTokenList.FirstWordType: TWordType;
Begin
  Result := wtNotAWord;
  If Count > 0 Then
    Result := First.WordType;
End;

Function TSourceTokenList.FirstSolidTokenType: TTokenType;
Var
  lc: TSourceToken;
Begin
  Result := ttUnknown;
  lc := FirstSolidToken;
  If lc <> Nil Then
    Result := lc.TokenType;
End;

Function TSourceTokenList.FirstSolidWordType: TWordType;
Var
  lc: TSourceToken;
Begin
  Result := wtNotAWord;
  lc := FirstSolidToken;
  If lc <> Nil Then
    Result := lc.WordType;
End;

Function TSourceTokenList.FirstTokenWithExclusion(Const AExclusions: TTokenTypeSet): TSourceToken;
Var
  liLoop: integer;
  lcItem: TSourceToken;
Begin
  Result := Nil;
  liLoop := FStackIndex;
  While liLoop < Count Do
  Begin
    lcItem := TSourceToken(List^[liLoop]);
    If Not (lcItem.TokenType In AExclusions) Then
    Begin
      Result := lcItem;
      break;
    End;
    Inc(liLoop);
  End;
End;

Function TSourceTokenList.Add(const pcToken: TSourceToken): Integer;
Begin
  Result := Inherited Add(pcToken);
End;

Function TSourceTokenList.FirstSolidToken: TSourceToken;
Begin
  Result := SolidToken(1);
End;

Function TSourceTokenList.SolidToken(piIndex: integer): TSourceToken;
Var
  liLoop: integer;
  lcTestToken: TSourceToken;
Begin
  Assert(piIndex > 0);
  Result := Nil;
  liLoop := FStackIndex;

  While liLoop < Count Do
  Begin
    lcTestToken := TSourceToken(List^[liLoop]);
    If (lcTestToken <> Nil) And lcTestToken.IsSolid Then
    Begin
      // found a solid token.
      
      If piIndex > 1 Then
        dec(piIndex) // go further
      Else
      Begin
        // found it
        Result := lcTestToken;
        break;
      End;
    End;
    Inc(liLoop);
  End;
End;

Function TSourceTokenList.SolidTokenType(const piIndex: integer): TTokenType;
Var
  lc: TSourceToken;
Begin
  Result := ttUnknown;
  lc := SolidToken(piIndex);
  If lc <> Nil Then
    Result := lc.TokenType;
End;

Function TSourceTokenList.SolidWordType(const piIndex: integer): TWordType;
Var
  lc: TSourceToken;
Begin
  Result := wtNotAWord;
  lc := SolidToken(piIndex);
  If lc <> Nil Then
    Result := lc.WordType;
End;

Procedure TSourceTokenList.SetXYPositions;
Var
  liLoop: integer;
  liX, liY: integer;
  lcToken: TSourceToken;
Begin
  liX := 1;
  liY := 1;
  liLoop := FStackIndex;
  While liLoop < count Do Begin
    lcToken := TSourceToken(List^[liLoop]);
    lcToken.XPosition := liX;
    lcToken.YPosition := liY;
    AdvanceTextPos(lcToken.SourceCode, liX, liY);
    Inc(liLoop);
  End;
End;

Function TSourceTokenList.Extract(const piIndex: Integer): TSourceToken;
Begin
  {Here I am not doing any index checking at all.
    This thing needs to be FAST. Access to here is quite controlled anyway.}
  Result := TSourceToken(List^[piIndex]);
  List^[piIndex] := Nil;
  FStackIndex := piIndex + 1;
End;

Procedure TSourceTokenList.Insert(const piIndex: Integer; const pcItem: TSourceToken);
Begin
  If FStackIndex <> 0 Then
    Raise Exception.Create('Insert Not allowed in Stack mode');

  Inherited Insert(piIndex, pcItem);
End;

Procedure TSourceTokenList.Delete(const piIndex: Integer);
Begin
  If FStackIndex <> 0 Then
    Raise Exception.Create('Delete Not allowed in Stack mode');

  Inherited Delete(piIndex);
End;

End.

