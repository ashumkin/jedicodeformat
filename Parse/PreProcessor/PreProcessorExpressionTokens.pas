unit PreProcessorExpressionTokens;

{
  AFS 26 Aug 2003

  Delphi preprocessor $IF expression parsing

  tokens defined via an enum, an item class
  and a list class
}


interface

uses Contnrs;

type

  TPreProcessorSymbol =
    (eNone, eIdentifier,
    // symbols with fixed text
    eOpenBracket, eCloseBracket, eDefined, eAnd, eOr, eNot, eTrue, eFalse);

{ used to recognise tokens - all expect identifiers have fixed text }
const
  SYMBOL_DATA: array[eOpenBracket .. eFalse] of string =
     ('(', ')', 'defined', 'and', 'or', 'not', 'true', 'false');

type

  TPreProcessorExpressionToken = class(TObject)
  private
    feSymbol: TPreProcessorSymbol;
    fsSourceCode: string;
  public
    property Symbol: TPreProcessorSymbol read feSymbol write feSymbol;
    property SourceCode: string read fsSOurceCode write fsSourceCode;
  end;

  TPreProcessorExpressionTokenList = class(TObject)
  private
    fcList: TObjectList;

    function GetItems(const piIndex: integer): TPreProcessorExpressionToken;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const peSymbol: TPreProcessorSymbol; psText: string): TPreProcessorExpressionToken;
    procedure Clear;

    property Items[const piIndex: integer]: TPreProcessorExpressionToken read GetItems;
    property Count:  integer read GetCount;
  end;

function PreProcessorSymbolToString(const peSymbol: TPreProcessorSymbol): string;

implementation

uses SysUtils;


function PreProcessorSymbolToString(const peSymbol: TPreProcessorSymbol): string;
begin
  case peSymbol of
    eNone:
      Result := 'No symbol';
    eIdentifier:
      Result := 'identifier';
    eOpenBracket:
      Result := '(';
    eCloseBracket:
      Result := ')';
    eDefined:
      Result := 'defined';
    eAnd:
      Result := 'and';
    eOr:
      Result := 'or';
    eNot:
      Result := 'not';
    eTrue:
      Result := 'true';
    eFalse:
      Result := 'false';
  else
    Assert(false);
  end;

end;

{ TPreProcessorExpressionTokenList }

function TPreProcessorExpressionTokenList.Add(const peSymbol: TPreProcessorSymbol; psText: string): TPreProcessorExpressionToken;
begin
  Result := TPreProcessorExpressionToken.Create;
  Result.Symbol := peSymbol;
  Result.SourceCode := psText;
  
  fcList.Add(Result);
end;

procedure TPreProcessorExpressionTokenList.Clear;
begin
  fcList.Clear;
end;

constructor TPreProcessorExpressionTokenList.Create;
begin
  inherited;
  // thiws is an owning list
  fcList := TObjectList.Create;
end;

destructor TPreProcessorExpressionTokenList.Destroy;
begin
  FreeAndNil(fcList);
  inherited;

end;

function TPreProcessorExpressionTokenList.GetCount: integer;
begin
  Result := fcList.Count;
end;

function TPreProcessorExpressionTokenList.GetItems(const piIndex: integer): TPreProcessorExpressionToken;
begin
  Result := TPreProcessorExpressionToken(fcList[piIndex]);
end;


end.

