unit PreProcessorEval;

{
  AFS 26 Aug 2003

  Delphi preprocessor $IF expression parsing

  Toplevel unit
}

interface


function EvalPreProcessorExpression(const psExpression: string): boolean;

implementation

uses
  SysUtils,
  PreProcessorTokenise, PreProcessorParse;

function EvalPreProcessorExpression(const psExpression: string): boolean;
var
  lcTokeniser: TPreProcessorTokeniser;
  lcParser: TPreProcessorParser;
begin
  Result := False;

  lcTokeniser := TPreProcessorTokeniser.Create;
  lcParser := TPreProcessorParser.Create;
  try
    // tokenise
    try
      lcTokeniser.Expression := psExpression;
      lcTokeniser.Tokenise;
    except
      on E: Exception do
        Raise Exception.Create('Exception tokenising "' + psExpression + '": ' + E.Message);
    end;

    // parse
    try
      lcParser.Tokens := lcTokeniser.Tokens;
      Result := lcParser.Parse;
    except
      on E: Exception do
        Raise Exception.Create('Exception parsing "' + psExpression + '": ' + E.Message);
    end;
  finally
    lcTokeniser.Free;
    lcParser.Free;
  end;
end;

end.
