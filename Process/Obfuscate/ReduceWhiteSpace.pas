unit ReduceWhiteSpace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReduceWhiteSpace, released May 2003.
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

{ AFS 28 Dec 2002

  Visitor to reduce all whitespace to single spaces
  Obfuscation
}

uses SwitchableVisitor, VisitParseTree;

type
  TReduceWhiteSpace = class(TSwitchableVisitor)
  protected
    procedure EnabledVisitSourceToken(const pcNode: TObject;
      var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;
  end;


implementation

uses SourceToken, Tokens, FormatFlags;

constructor TReduceWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

procedure TReduceWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
var
  lcSourceToken: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttWhiteSpace then
    lcSourceToken.SourceCode := ' ';
end;

end.
