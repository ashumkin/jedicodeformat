unit RebreakLines;

{ AFS 29 December 2002

  Obfuscate process
  break lines at regular intervals
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RebreakLines, released May 2003.
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
  TRebreakLines = class(TSwitchableVisitor)
  private
    xPos: integer;
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;

implementation

uses
  JclStrings,
  SourceToken, Tokens, FormatFlags, TokenUtils;

constructor TRebreakLines.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
  xPos := 1;
end;

procedure TRebreakLines.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
const
  LINE_LENGTH = 80;
var
  lcToken: TSourceToken;
  lcNext, lcNew: TSourceToken;
  liLen: integer;
begin
  lcToken := TSourceToken(pcNode);


  if lcToken.TokenType = ttReturn then
    xPos := 0
  else
  begin
    liLen := Length(lcToken.SourceCode);

    if (XPos + liLen) > LINE_LENGTH then
    begin
      { no space directly after the new return }
      lcNext := lcToken.NextToken;
      if (lcNext <> nil) and (lcNext.TokenType = ttWhiteSpace) and (lcNext.SourceCode <> '') then
          BlankToken(lcNext);

      { need a return? }
      if (lcNext <> nil) and (lcNext.TokenType <> ttReturn) then
      begin
        prVisitResult.Action := aInsertAfter;

        lcNew := TSourceToken.Create;
        lcNew.TokenType := ttReturn;
        lcNew.SourceCode := AnsiLineBreak;
        XPos := 0;

        prVisitResult.NewItem := lcNew;
      end;
    end
    else
      // not at enhd of line yet 
      xPos := xPos + liLen;
  end;
end;

end.