unit Preprocessor;

{ AFS 22 August 2003
  this class works on a token list
  and hides code that is ifdef'd out from the parser
  that will turn this list into a tree
  }

interface

uses
  { delphi }
  Classes,
  { local }
  SourceToken, SourceTokenList;

type
  TPreProcessor = class(TObject)
  private
    // working data
    fiStartNestingLevel: integer;
    fcDefinedSymbols: TStringList;

    // referenced data
    fcTokenList: TSourceTokenList;


    function RemoveBlock(const piIndex, piStartNestingLevel: integer;
      const pbStopAtElse: Boolean): Boolean;

    procedure RemoveDefinedSymbol(const psSymbol: string);
    function SymbolIsDefined(const psSymbol: string): boolean;

    function BlockStartIsIncluded(const pcToken: TSourceToken): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessTokenList;

    // used in the more complex $IF syntax
    procedure AddDefinedSymbol(const psSymbol: string);
    function EvalPreProcessorExpression(const psExpression: string): boolean;


    property TokenList: TSourceTokenList read fcTokenList write fcTokenList;
  end;

procedure RemoveConditionalCompilation(const pcTokenList: TSourceTokenList);

implementation

uses
  SysUtils,
  JclStrings, Tokens, JcfSettings,
  PreProcessorTokenise, PreProcessorParse;


type
  TPreProcessorSymbolType = (btNone,
    btDefine, btUndef,
    bcIfDef, bcIfNotDef, bcIfOpt, bcIfExpr, bcElseIf,
    btBlockElse, btBlockEnd, btNewBlockEnd);

  TPreProcessorSymbolTypeSet = set of TPreProcessorSymbolType;


const
  SymbolData: array[TPreProcessorSymbolType] of string =(
    '$$$$$$$$$$',
    '{$DEFINE',
    '{$UNDEF',
    '{$IFDEF',
    '{$IFNDEF',
    '{$IFOPT',
    '{$IF',
    '{$ELSEIF',
    '{$ELSE',
    '{$ENDIF',
    '{$IFEND'
  );

const
  BLOCK_START: TPreProcessorSymbolTypeSet = [bcIfDef, bcIfNotDef, bcIfOpt, bcIfExpr, bcElseIf];
  BLOCK_ELSE: TPreProcessorSymbolTypeSet = [btBlockElse, bcElseIf];
  BLOCK_END: TPreProcessorSymbolTypeSet = [btBlockEnd, btNewBlockEnd];

function GetPreprocessorSymbolType(const pcToken: TSourceToken): TPreProcessorSymbolType;
var
  leLoop: TPreProcessorSymbolType;
  liItemLen: integer;
begin
  Assert(pcToken <> nil);

  Result := btNone;

  if pcToken.CommentStyle <> eCompilerDirective then
    exit;

  for leLoop := low(TPreProcessorSymbolType) to High(TPreProcessorSymbolType) do
  begin
    if leLoop = btNone then
      continue;

    liItemLen := Length(SymbolData[leLoop]);
    if AnsiSameText(StrLeft(pcToken.SourceCode, liItemLen), SymbolData[leLoop]) and
      (not CharIsAlpha(pcToken.SourceCode[liItemLen + 1])) then
    begin
      Result := leLoop;
      break;
    end;

  end;
end;


function GetPreprocessorCondition(const pcToken: TSourceToken): string;
var
  leSymbolType: TPreProcessorSymbolType;
begin
  Result := '';

  leSymbolType := GetPreprocessorSymbolType(pcToken);

  if leSymbolType = btNone then
    exit;

  Result := StrRestOf(pcToken.SourceCode, Length(SymbolData[leSymbolType]) + 1);

  if Result <> '' then
  begin
    if StrRight(Result, 1) = '}' then
      Result := StrChopRight(result, 1);

    Result := Trim(Result);
  end;
end;


procedure RemoveConditionalCompilation(const pcTokenList: TSourceTokenList);
var
  lcComp: TPreProcessor;
begin
  Assert(pcTokenList <> nil);

  lcComp := TPreProcessor.Create;
  try
    lcComp.TokenList := pcTokenList;
    lcComp.ProcessTokenList;
  finally
    lcComp.Free;
  end;
end;

constructor TPreProcessor.Create;
begin
  inherited;

  fcDefinedSymbols := TStringList.Create;
  fcDefinedSymbols.Sorted := True;

  fcDefinedSymbols.Assign(FormatSettings.PreProcessor.DefinedSymbols);
end;

destructor TPreProcessor.Destroy;
begin
  FreeAndNil(fcDefinedSymbols);

  inherited;
end;

procedure TPreProcessor.AddDefinedSymbol(const psSymbol: string);
begin
  if (psSymbol <> '') and (not SymbolIsDefined(psSymbol)) then
    fcDefinedSymbols.Add(psSymbol);
end;

procedure TPreProcessor.RemoveDefinedSymbol(const psSymbol: string);
var
  liIndex: integer;
begin
  liIndex := fcDefinedSymbols.IndexOf(psSymbol);
  if liIndex >= 0 then
    fcDefinedSymbols.Delete(liIndex);
end;

function TPreProcessor.SymbolIsDefined(const psSymbol: string): boolean;
begin
  Result := fcDefinedSymbols.IndexOf(psSymbol) >= 0;
end;

{ each compiler option has an inverse
  the inverse of comiler flag "X+" is "X-" and vice versa
}
function InverseOption(const psOption: string): string;
begin
  // swap + fro minus to find the inverse
  Result := StrReplaceChar(psOption, '+', '?');
  Result := StrReplaceChar(Result, '-', '+');
  Result := StrReplaceChar(Result, '?', '-');
end;

function TPreProcessor.BlockStartIsIncluded(const pcToken: TSourceToken): Boolean;
var
  leBlockCond: TPreProcessorSymbolType;
  lsSymbol: string;
begin
  Assert(pcToken <> nil);
  Result := False;

  leBlockCond := GetPreprocessorSymbolType(pcToken);

  if (leBlockCond in BLOCK_START) then
  begin
    lsSymbol := GetPreprocessorCondition(pcToken);

    case leBlockCond of
      bcIfDef:
        Result := SymbolIsDefined(lsSymbol);
      bcIfNotDef:
        Result := not SymbolIsDefined(lsSymbol);
      bcIfExpr:
        Result := EvalPreProcessorExpression(lsSymbol);
      bcIfOpt:
        Result := FormatSettings.PreProcessor.OptionIsDefined(lsSymbol);
      else
        Assert(False);
    end;
  end;
end;



{ scan from the else to the end, and hide it all! }
function TPreProcessor.RemoveBlock(
  const piIndex, piStartNestingLevel: integer; const pbStopAtElse: Boolean): boolean;
var
  liLoop: integer;
  liEndIndex: integer;
  liCurrentNestingLevel: integer;
  lcToken: TSourceToken;
  lcStoredItems: TSourceToken;
  leSymbolType: TPreProcessorSymbolType;
begin
  Result := False;
  liEndIndex := -1;
  liCurrentNestingLevel := piStartNestingLevel;

  for liLoop := piIndex + 1 to fcTokenList.Count - 1 do
  begin
    lcToken := fcTokenList.SourceTokens[liLoop];

    leSymbolType := GetPreprocessorSymbolType(lcToken);

    if leSymbolType in BLOCK_START then
          inc(liCurrentNestingLevel);

    if leSymbolType in BLOCK_ELSE then
    begin
      if pbStopAtElse and (liCurrentNestingLevel = piStartNestingLevel) then
      begin
        liEndIndex := liLoop;
        break;
      end;
    end;

    if leSymbolType in BLOCK_END then
    begin
      if liCurrentNestingLevel = piStartNestingLevel then
      begin
        liEndIndex := liLoop;
        break;
      end;
      dec(liCurrentNestingLevel);
    end
  end;

  if liEndIndex >= 0 then
  begin
    lcStoredItems := TSourceToken.Create;
    lcStoredItems.TokenType := ttConditionalCompilationRemoved;


    for liLoop := liEndIndex downto piIndex do
    begin
      lcStoredItems.SourceCode := fcTokenList.SourceTokens[liLoop].SourceCode + lcStoredItems.SourceCode;
      fcTokenList.Remove(liLoop);
    end;

    fcTokenList.Insert(piIndex, lcStoredItems);
    Result := True;
  end;
end;


{ TPreProcessor }

procedure TPreProcessor.ProcessTokenList;
var
  liLoop: integer;
  lcToken: TSourceToken;
  leSymbolType: TPreProcessorSymbolType;
begin
  Assert(TokenList <> nil);

  liLoop := 0;
  while liLoop < TokenList.Count - 1 do
  begin
    lcToken := TokenList.SourceTokens[liLoop];

    leSymbolType := GetPreprocessorSymbolType(lcToken);

    if leSymbolType = btDefine then
      AddDefinedSymbol(GetPreprocessorCondition(lcToken))
    else if leSymbolType = btUndef then
      RemoveDefinedSymbol(GetPreprocessorCondition(lcToken));

    if leSymbolType in BLOCK_START then
    begin
      inc(fiStartNestingLevel);
      if not BlockStartIsIncluded(lcToken) then
      begin
        RemoveBlock(liLoop, fiStartNestingLevel, True);
      end;
    end;

    if leSymbolType in BLOCK_ELSE then
    begin
      if RemoveBlock(liLoop, fiStartNestingLevel, (leSymbolType = bcElseIf)) then
        dec(fiStartNestingLevel);
    end;

    if leSymbolType in BLOCK_END then
      dec(fiStartNestingLevel);

    inc(liLoop);
  end;
end;

function TPreProcessor.EvalPreProcessorExpression(
  const psExpression: string): boolean;
var
  lcTokeniser: TPreProcessorTokeniser;
  lcParser: TPreProcessorParser;
begin
  Result := False;
  Assert(psExpression <> '');

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

    { !! unknown syntax. Accept expression as true ? fix in later version }
    if lcTokeniser.HasError then
    begin
      Result := True;
    end
    else
    begin
                                             
      // parse
      try
        lcParser.Tokens := lcTokeniser.Tokens;
        lcParser.DefinedSymbols := fcDefinedSymbols;

        Result := lcParser.Parse;
      except
        on E: Exception do
          Raise Exception.Create('Exception parsing "' + psExpression + '": ' + E.Message);
      end;
    end;
  finally
    lcTokeniser.Free;
    lcParser.Free;
  end;
end;

end.
