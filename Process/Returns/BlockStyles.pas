unit BlockStyles;

{ AFS 22 April
  This unit handles the different styles of line breaking & spacing after the constructs
  if <expression> then
    statement;

  if <expression> then
  begin
     ..statements
  end;

  The styles are
  - never break line (subject to approval by the linebreaker.
    If the resulting line is too long, just after the then
    is a very good place to break and may be chosen. )
  - Leave as is
  - Always break line. This is the official style
    http://www.borland.com/techvoyage/articles/DelphiStyle/StyleGuide.html

  This style also applies to
    for <expression> do
  and
    while <expression> do
  and
    else
      <statement>

 to do:
 apply to

 case exp of
  value: statement;
  value: begin statements end;

 end;
}


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is BlockStyles, released May 2003.
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

uses SwitchableVisitor, VisitParseTree, ParseTreeNodeType;


type
  TBlockStyles = class(TSwitchableVisitor)
    private

    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;

implementation

uses Tokens, SourceToken, TokenUtils, JCFSettings,
  FormatFlags, SettingsTypes;

const
  BreakWords: TTokenTypeSet = [ttThen, ttDo, ttElse, ttEnd];

function GetStyle(const pt: TSourceToken): TBlockNewLineStyle;
var
  lcNextToken: TSourceToken;
begin
  Result := eLeave;

  lcNextToken := pt.NextSolidToken;
  if lcNextToken = nil then
    exit;

  { only do anything to an end if it is followed by an else }
  if pt.TokenType = ttEnd then
  begin
    if lcNextToken.TokenType = ttElse then
      Result := FormatSettings.Returns.EndElseStyle;
  end
  else if pt.TokenType = ttColon then
  begin
    if IsCaseColon(pt) then
    begin
      if lcNextToken.TokenType = ttBegin then
        // always a return here
        Result := eAlways
      else
        Result := FormatSettings.Returns.CaseLabelStyle;
    end
    else if IsLabelColon(pt) then
    begin
      { otherwise, is there a begin next? }
      if lcNextToken.TokenType = ttBegin then
        Result := FormatSettings.Returns.LabelBeginStyle
      else
        Result := FormatSettings.Returns.LabelStyle;
    end;
  end
  else if (pt.TokenType = ttElse) and (not pt.HasParentNode(nElseCase, 1)) and
    (lcNextToken.TokenType = ttIf) then
  begin
    { though else normally starts a block,
     according to standards, there is never a return in "else if" (!!! check!)
     But we have a config setting for Marcus F

      **NB** rare exception: this does not apply when the if is not related to the else
      ie
       case (foo) of
         1:
          DoSomething1;
         2:
          SoSomething2;
         else
           // this is the else case, not part of an if.
           // All statements from the 'else' to the 'end' form a block
           if (SomeCond) then // though the 'if' is directly after the else, this is not an else-if
             DoSomethingElse;
           if (SomeOtherCond) then
             DoSomeOtherThing;
       end;

       end;

     }
    Result := FormatSettings.Returns.ElseIfStyle;
  end
  else
  begin
    { otherwise, is there a begin next? }
    if lcNextToken.TokenType = ttBegin then
      Result := FormatSettings.Returns.BlockBeginStyle
    else
      Result := FormatSettings.Returns.BlockStyle;
  end;
end;


constructor TBlockStyles.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eBlockStyle, eAddReturn, eRemoveReturn];
end;

procedure TBlockStyles.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  leStyle: TBlockNewLineStyle;
  lcSourceToken, lcNextReturn: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType in BreakWords) or IsLabelColon(lcSourceToken) or IsCaseColon(lcSourceToken) then
  begin
    leStyle := GetStyle(lcSourceToken);

    case leStyle of
      eLeave:; // do nothing, leave as is
      eAlways:
      begin
        lcNextReturn := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttComment]);
        if (lcNextReturn <> nil) and (lcNextReturn.TokenType <> ttReturn) then
        begin
          prVisitResult.Action := aInsertAfter;
          prVisitResult.NewItem := NewReturn;
        end;
      end;
      eNever:
      begin
        lcNextReturn := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttComment]);
        if (lcNextReturn <> nil) and (lcNextReturn.TokenType = ttReturn) then
        begin
          // turn to space
          lcNextReturn.TokenType := ttWhiteSpace;
          // need some space here - don't leave nothing between tokens
          if (lcNextReturn.PriorToken.TokenType = ttWhiteSpace) or
            (lcNextReturn.NextToken.TokenType = ttWhiteSpace) then

          // null space as it's next to space already
          lcNextReturn.SourceCode := ''
          else
            // keep a space 
            lcNextReturn.SourceCode := ' ';
        end;
      end;
      else
        Assert(False);
    end;
  end;
end;

end.