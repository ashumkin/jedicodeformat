unit PreProcessorExpressionTokenise;

{
  AFS 26 Aug 2003

 lexer for preprocessor $IF expressions
 Turns text into a list of tokens
 The tokens are defined in PreProcessorTokens
 Whitespace is discarded
}

interface

uses PreProcessorExpressionTokens;

type
  TPreProcessorExpressionTokeniser = class
  private
    fsExpr: string;
    fiCurrentIndex: integer;
    fbHasError: Boolean;

    fcTokens: TPreProcessorExpressionTokenList;

    function Rest: string;
    function StartsWith(const ps: string): Boolean;

    function TryConsumeFixedSymbol: boolean;
    function TryConsumeIdentifier: boolean;
    procedure ConsumeWhiteSpace;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Tokenise;

    property Expression: string read fsExpr write fsExpr;
    property Tokens: TPreProcessorExpressionTokenList read fcTokens;
    property HasError: boolean read fbHasError;
  end;

implementation

uses
  SysUtils,
  JclStrings;


constructor TPreProcessorExpressionTokeniser.Create;
begin
  inherited;
  fcTokens := TPreProcessorExpressionTokenList.Create;
end;

destructor TPreProcessorExpressionTokeniser.Destroy;
begin
  FreeAndNil(fcTokens);
  inherited;
end;

function TPreProcessorExpressionTokeniser.Rest: string;
begin
  Result := StrRestOf(fsExpr, fiCurrentIndex);
end;

function TPreProcessorExpressionTokeniser.StartsWith(const ps: string): Boolean;
begin
  Result := AnsiSameText(StrLeft(Rest, length(ps)), ps);
end;

procedure TPreProcessorExpressionTokeniser.Tokenise;
begin
  fcTokens.Clear;
  fiCurrentIndex := 1;
  fbHasError := False;

  while fiCurrentIndex <= Length(fsExpr) do
  begin
    if not TryConsumeFixedSymbol then
      if not TryConsumeIdentifier then
      begin
        // unknown/unsupported Syntax. :(
        fbHasError := True;
        break;
      end;

    ConsumeWhiteSpace;
  end;

end;

function TPreProcessorExpressionTokeniser.TryConsumeFixedSymbol: boolean;
var
  leLoop: TPreProcessorSymbol;
  lbFound: boolean;
begin
  result := False;

  for leLoop := low(SYMBOL_DATA) to high(SYMBOL_DATA) do
  begin
    lbFound := StartsWith(SYMBOL_DATA[leLoop]);

    if lbFound then
    begin
      fcTokens.Add(leLoop, SYMBOL_DATA[leLoop]);

      fiCurrentIndex := fiCurrentIndex + Length(SYMBOL_DATA[leLoop]);
      Result := True;
      break;
    end;
  end;
end;


function TPreProcessorExpressionTokeniser.TryConsumeIdentifier: boolean;
var
  liStart: integer;
  lsIdentifierText: string;
begin
  Result := False;

  if CharIsAlpha(fsExpr[fiCurrentIndex]) then
  begin
    liStart := fiCurrentIndex;
    while CharIsAlphaNum(fsExpr[fiCurrentIndex]) do
      inc(fiCurrentIndex);

    Result := True;

    lsIdentifierText := copy(fsExpr, liStart, fiCurrentIndex - liStart);
    fcTokens.Add(eIdentifier, lsIdentifierText);
  end;
end;


procedure TPreProcessorExpressionTokeniser.ConsumeWhiteSpace;
begin
  // this lexer can ignore the white space
  while (fiCurrentIndex < Length(fsExpr)) and CharIsWhiteSpace(fsExpr[fiCurrentIndex]) do
    inc(fiCurrentIndex);
end;

end.