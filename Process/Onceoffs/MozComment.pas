{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is MozComment.pas, released April 2000.
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

unit MozComment;

{ AFS 24 march 2K
 The Mozilla public licence requires that all files include a header comment
 that specifies the licence.

 Right now, my files don't
 The easiest way to fix that, is this code below:
}

interface

uses SwitchableVisitor, SourceToken, VisitParseTree;

type
  TMozComment = class(TSwitchableVisitor)
  private
    bHasMoz: boolean;
  protected

    procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { delphi } SysUtils,
  JclStrings,
  { local } WordMap, JcfSettings, TokenType, SetClarify;


const
  { this comment will be inserted in all files above the unit header }
  MozURL = 'http://www.mozilla.org/NPL/';

  MozCommentString: string =
    NOFORMAT_ON + AnsiLineBreak + // so this program can't easily obfuscate it out
    '(*------------------------------------------------------------------------------' +
    AnsiLineBreak +
    ' Delphi Code formatter source code ' + AnsiLineBreak + AnsiLineBreak +
    'The Original Code is <FileName>, released <Date>.' + AnsiLineBreak +
    'The Initial Developer of the Original Code is Anthony Steele. ' + AnsiLineBreak +
    'Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.' +
    AnsiLineBreak +
    'All Rights Reserved. ' + AnsiLineBreak +
    'Contributor(s): Anthony Steele. ' + AnsiLineBreak + AnsiLineBreak +
    'The contents of this file are subject to the Mozilla Public License Version 1.1' +
    AnsiLineBreak +
    '(the "License"). you may not use this file except in compliance with the License.' +
    AnsiLineBreak +
    'You may obtain a copy of the License at ' + MozURL + AnsiLineBreak + AnsiLineBreak +
    'Software distributed under the License is distributed on an "AS IS" basis,' +
    AnsiLineBreak +
    'WITHOUT WARRANTY OF ANY KIND, either express or implied.' + AnsiLineBreak +
    'See the License for the specific language governing rights and limitations ' +
    AnsiLineBreak +
    'under the License.' + AnsiLineBreak +
    '------------------------------------------------------------------------------*)' +
    AnsiLineBreak + NOFORMAT_OFF + AnsiLineBreak + AnsiLineBreak;


  { TMozComment }

constructor TMozComment.Create;
begin
  inherited;
  bHasMoz := False;
end;

function TMozComment.IsIncludedInSettings: boolean;
begin
  Result := (not FormatSettings.Obfuscate.Enabled) and
    (FormatSettings.Clarify.OnceOffs <> eDoNotRun)
end;


procedure TMozComment.EnabledVisitSourceToken(const pcNode: TObject;
  var prVisitResult: TRVisitResult);
const
  UnitStart: TWordSet = [wUnit, wProgram, wLibrary];
var
  lsFile:    string;
  lsComment: string;
  lcToken, lcNewComment: TSourceToken;
  lbInContext: boolean;
begin
  lcToken := TSourceToken(pcNode);

  lbInContext := ((lcToken.Word in UnitStart) or (lcToken.TokenType = ttComment)) and not bHasMoz;
  if not lbInContext then
    exit;

  if (lcToken.TokenType = ttComment) then
  begin
    { check for existing Moz. comment
     Any comment with the Moz. licence URL is assumed to be it }

    if Pos(MozURL, lcToken.SourceCode) > 0 then
      bHasMoz := True;
  end
  else
  begin
    { get the file name but remove the path
      This will be inserted into the standard comment string
    }

    //lsFile := ExtractFileName(OriginalFileName);

    lsComment := MozCommentString;
    lsComment := StringReplace(lsComment, '<FileName>', lsFile, [rfReplaceAll]);
    lsComment := StringReplace(lsComment, '<Date>', FormatDateTime('mmmm yyyy', Date),
      [rfReplaceAll]);

    // put the comment in front of the unit start word
    lcNewComment := TSourceToken.Create;
    lcNewComment.TokenType := ttComment;
    lcNewComment.SourceCode := lsComment;

    lcToken.Parent.InsertChild(lcToken.IndexOfSelf, lcNewComment);

    bHasMoz := True;
  end;
end;

end.
