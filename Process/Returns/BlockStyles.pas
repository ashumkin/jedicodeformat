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


interface

uses SwitchableVisitor, VisitParseTree, ParseTreeNodeType;


type
  TBlockStyles = class(TSwitchableVisitor)
    private
      fbRemoveNextReturn: boolean;

    protected
      procedure EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult); override;
    public
      constructor Create; override;
  end;

implementation

uses WordMap, SourceToken, TokenType, TokenUtils, JCFSettings,
  FormatFlags;

const
  BreakWords: TWordSet = [wThen, wDo, wElse, wEnd];

function GetStyle(const pt: TSourceToken): TBlockNewLineStyle;
var
  lcNextToken: TSourceToken;
begin
  Result := eLeave;

  lcNextToken := pt.NextSolidToken;
  if lcNextToken = nil then
    exit;

  { only do anything to an end if it is followed by an else }
  if pt.Word = wEnd then
  begin
    if lcNextToken.Word = wElse then
      Result := Settings.Returns.EndElseStyle;
  end
  else if pt.TokenType = ttColon then
  begin
    if IsCaseColon(pt) then
    begin
      if lcNextToken.Word = wBegin then
        // always a return here
        Result := eAlways
      else
        Result := Settings.Returns.CaseLabelStyle;
    end
    else if IsLabelColon(pt) then
    begin
      { otherwise, is there a begin next? }
      if lcNextToken.Word = wBegin then
        Result := Settings.Returns.LabelBeginStyle
      else
        Result := Settings.Returns.LabelStyle;
    end;
  end
  else if (pt.Word = wElse) and (not pt.HasParentNode(nElseCase, 1)) and (lcNextToken.Word = wIf) then
  begin
    { though else normally starts a block,
     there is never a return in "else if"
     If this is an issue, make a config setting later

      **NB** rare exception: this does not apply when the if is not related to the else
      ie
       case (foo) of
         1:
          DoSomething;
         2:
          SoSomethingElse;
         else
           // this is the else case, not part of an if.
           // All statements from the 'else' to the 'end' form a block
           if (SomeCond) then // though the is is directly after the else, this is not an else-if
             DoSomething;
           if (SomeOtherCond) then
             DoSomeOtherThing;
       end;

       end;

     }
    Result := eNever;
  end
  else
  begin
    { otherwise, is there a begin next? }
    if lcNextToken.Word = wBegin then
      Result := Settings.Returns.BlockBeginStyle    
    else
      Result := Settings.Returns.BlockStyle;
  end;
end;


constructor TBlockStyles.Create;
begin
  inherited;
  fbRemoveNextReturn := False;
  FormatFlags := FormatFlags + [eBlockStyle, eAddReturn, eRemoveReturn];
end;

procedure TBlockStyles.EnabledVisitSourceToken(const pcNode: TObject; var prVisitResult: TRVisitResult);
var
  leStyle: TBlockNewLineStyle;
  lcSourceToken, lcNextReturn: TSourceToken;
begin
  lcSourceToken := TSourceToken(pcNode);

  if fbRemoveNextReturn and (lcSourceToken.TokenType = ttReturn) then
  begin
    prVisitResult.Action := aDelete;
    fbRemoveNextReturn := False; // done it now
  end;


  if not (lcSourceToken.Word in BreakWords) or IsLabelColon(lcSourceToken) or IsCaseColon(lcSourceToken) then
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
          // set it up to remove when we get there
          fbRemoveNextReturn := True;
      end;
      else
        Assert(False);
    end;
  end;
end;

end.
