unit PreProcessorTokens;

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

  TPreProcessorToken = class(TObject)
  private
    feSymbol: TPreProcessorSymbol;
    fsSourceCode: string;
  public
    property Symbol: TPreProcessorSymbol read feSymbol write feSymbol;
    property SourceCode: string read fsSOurceCode write fsSourceCode;
  end;

  TPreProcessorTokenList = class(TObject)
  private
    fcList: TObjectList;

    function GetItems(const piIndex: integer): TPreProcessorToken;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const peSymbol: TPreProcessorSymbol; psText: string): TPreProcessorToken;
    procedure Clear;

    property Items[const piIndex: integer]: TPreProcessorToken read GetItems;
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

{ TPreProcessorTokenList }

function TPreProcessorTokenList.Add(const peSymbol: TPreProcessorSymbol; psText: string): TPreProcessorToken;
begin
  Result := TPreProcessorToken.Create;
  Result.Symbol := peSymbol;
  Result.SourceCode := psText;
  
  fcList.Add(Result);
end;

procedure TPreProcessorTokenList.Clear;
begin
  fcList.Clear;
end;

constructor TPreProcessorTokenList.Create;
begin
  inherited;
  // thiws is an owning list
  fcList := TObjectList.Create;
end;

destructor TPreProcessorTokenList.Destroy;
begin
  FreeAndNil(fcList);
  inherited;

end;

function TPreProcessorTokenList.GetCount: integer;
begin
  Result := fcList.Count;
end;

function TPreProcessorTokenList.GetItems(const piIndex: integer): TPreProcessorToken;
begin
  Result := TPreProcessorToken(fcList[piIndex]);
end;


end.

