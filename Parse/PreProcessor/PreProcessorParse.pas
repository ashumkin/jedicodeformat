unit PreProcessorParse;

{
  AFS 26 Aug 2003

  Delphi preprocessor $IF expression parsing

  an 'immediate' parser that evalutes the expression,
  does not produce a parse tree except in the ephemera of the call stack

  All these expression are in $IF preprocessor statements
  and thus have boolean results


 Grammar:

 expr -> term
 expr -> term and expr
 expr -> term or expr

 term -> (expr)
 term -> defined(identifier)
 term -> not term
 term -> true
 term -> false

 and that's all for now. Just "defined" checks, with brackets, negation and conjunctions
 More will come later, such as '=' '<' etc.
 But that will necessitate symbols with values and some kind of type inference
}

interface

uses PreProcessorTokens;

type
  TPreProcessorParser = class(TObject)
  private
    fiCurrentIndex: integer;
    fcTokens: TPreProcessorTokenList;

    function ParseExpr: Boolean;
    function ParseTerm: Boolean;

    function CurrentTokenType: TPreProcessorSymbol;
    function MoreTokens: Boolean;

    procedure Consume(const peType: TPreProcessorSymbol);
    function SymbolIsDefined(const psSymbol: string): Boolean;
  public
    function Parse: Boolean;

    property Tokens: TPreProcessorTokenList read  fcTokens write fcTokens;
  end;


implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfSettings;

{ TPreProcessorParser }

procedure TPreProcessorParser.Consume(const peType: TPreProcessorSymbol);
begin
  Assert(CurrentTokenType = peType,
    'expected token ' + PreProcessorSymbolToString(peType) +
    ' got ' +  PreProcessorSymbolToString(CurrentTokenType) +
    ' at position ' + IntToStr(fiCurrentIndex));
  inc(fiCurrentIndex)
end;

function TPreProcessorParser.CurrentTokenType: TPreProcessorSymbol;
begin
  Result := fcTokens.Items[fiCurrentIndex].Symbol;
end;

function TPreProcessorParser.MoreTokens: Boolean;
begin
  Result := fcTokens.Count > fiCurrentIndex;
end;

function TPreProcessorParser.Parse: Boolean;
begin
  Assert(fcTokens <> nil);
  Assert(fcTokens.Count > 0);
  fiCurrentIndex := 0;

  Result := ParseExpr;

  Assert(not MoreTokens, 'Expression has trailing tokens');
end;

function TPreProcessorParser.ParseExpr: Boolean;
var
  lbExprResult: Boolean;
begin
  Result := ParseTerm;

  if MoreTokens then
  begin
    case CurrentTokenType of
      eAnd:
      begin
        Consume(eAnd);
        // always evaluate this
        lbExprResult := ParseExpr;
        Result := Result and lbExprResult;
      end;
      eOr:
      begin
        Consume(eOr);
        // always evaluate this
        lbExprResult := ParseExpr;
        Result := Result or lbExprResult;
      end;
      eCloseBracket:
      begin
        // do nothing, should be matched to open bracket below
      end;
      else
      begin
        Result := False;
        Assert(False, 'Preprocessor expression could not be parsed');
      end;
    end;
  end;
end;

function TPreProcessorParser.ParseTerm: Boolean;
begin

  case CurrentTokenType of
    eOpenBracket:
    begin
      Consume(eOpenBracket);
      Result := ParseExpr;
      Consume(eCloseBracket);
    end;
    eDefined:
    begin
      Consume(eDefined);
      Consume(eOpenBracket);
      Result := SymbolIsDefined(Tokens.Items[fiCurrentIndex].SourceCode);
      Consume(eIdentifier);
      Consume(eCloseBracket);
    end;
    eNot:
    begin
      Consume(eNot);
      Result := not ParseTerm;
    end;
    eTrue:
    begin
      Consume(eTrue);
      Result := True;
    end;
    eFalse:
    begin
      Consume(eFalse);
      Result := False;
    end;
    else
    begin
      Result := False;
      Assert(False, 'Preprocessor term could not be parsed');
    end;
  end;
end;


function TPreProcessorParser.SymbolIsDefined(const psSymbol: string): Boolean;
begin
  // should also keep changes during files
  Result := FormatSettings.PreProcessor.SymbolIsDefined(psSymbol);
end;

end.
