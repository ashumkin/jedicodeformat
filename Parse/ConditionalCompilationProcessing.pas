unit ConditionalCompilationProcessing;

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
  TConditionalCompilationProcessing = class(TObject)
  private
    // working data
    fiStartNestingLevel: integer;
    fcDefinedSymbols: TStringList;
    fcDefinedOptions: TStringList;


    // referenced data
    fcTokenList: TSourceTokenList;


    function RemoveBlock(const piIndex, piStartNestingLevel: integer;
      const pbStopAtElse: Boolean): Boolean;

    procedure AddDefinedSymbol(const psSymbol: string);
    procedure RemoveDefinedSymbol(const psSymbol: string);
    function SymbolIsDefined(const psSymbol: string): boolean;

    //procedure AddDefinedOption(const psOption: string);
    function OptionIsDefined(const psOption: string): boolean;

    function BlockStartIsIncluded(const pcToken: TSourceToken): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessTokenList;

    property TokenList: TSourceTokenList read fcTokenList write fcTokenList;
  end;

procedure RemoveConditionalCompilation(const pcTokenList: TSourceTokenList);

implementation

uses
  SysUtils,
  JclStrings, Tokens;


type
  TBlockMarkerType = (btNone, btBlockStart, btBlockElse, btBlockEnd);
  TBlockCondition = (bcNone, bcIfDef, bcIfNotDef, bcIfOpt);

function GetConditionalBlockType(const pcToken: TSourceToken): TBlockMarkerType;
begin
  Result := btNone;

  if pcToken.CommentStyle = eCompilerDirective then
  begin
    if StrHasPrefix(pcToken.SourceCode, ['{$IF ', '{$IFDEF', '{$IFNDEF', '{$IFOPT']) then
      Result := btBlockStart
    else if StrHasPrefix(pcToken.SourceCode, ['{$ELSE']) then
      Result := btBlockElse
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFEND', '{$ENDIF']) then
      Result := btBlockEnd;
  end;
end;

function GetBlockCondition(const pcToken: TSourceToken): TBlockCondition;
begin
  Result :=  bcNone;

  if pcToken.CommentStyle = eCompilerDirective then
  begin
    if StrHasPrefix(pcToken.SourceCode, ['{$IF']) then
      Result := bcIfDef
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFDEF']) then
      Result := bcIfDef
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFNDEF']) then
      Result := bcIfNotDef
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFOPT']) then
      Result := bcIfOpt;
  end;
end;

function GetConditionSymbol(const pcToken: TSourceToken): string;
begin
  Result := '';

  if pcToken.CommentStyle = eCompilerDirective then
  begin
    if StrHasPrefix(pcToken.SourceCode, ['{$IF ']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$IF') + 1)
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFDEF']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$IFDEF') + 1)
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFNDEF']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$IFNDEF')+ 1)
    else if StrHasPrefix(pcToken.SourceCode, ['{$IFOPT']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$IFOPT')+ 1)
    else if StrHasPrefix(pcToken.SourceCode, ['{$DEFINE']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$DEFINE')+ 1)
    else if StrHasPrefix(pcToken.SourceCode, ['{$UNDEF']) then
      Result := StrRestOf(pcToken.SourceCode, Length('{$UNDEF')+ 1);

    if Result <> '' then
    begin
      if StrRight(Result, 1) = '}' then
        Result := StrChopRight(result, 1);

      Result := Trim(Result);
    end;
  end;
end;


procedure RemoveConditionalCompilation(const pcTokenList: TSourceTokenList);
var
  lcComp: TConditionalCompilationProcessing;
begin
  Assert(pcTokenList <> nil);

  lcComp := TConditionalCompilationProcessing.Create;
  try
    lcComp.TokenList := pcTokenList;
    lcComp.ProcessTokenList;
  finally
    lcComp.Free;
  end;
end;

constructor TConditionalCompilationProcessing.Create;
begin
  inherited;

  fcDefinedSymbols := TStringList.Create;
  fcDefinedSymbols.Sorted := True;
  fcDefinedSymbols.Add('MSWINDOWS');
  fcDefinedSymbols.Add('DELPHI5_UP');

  fcDefinedOptions := TStringList.Create;
  fcDefinedOptions.Sorted := True;

end;

destructor TConditionalCompilationProcessing.Destroy;
begin
  FreeAndNil(fcDefinedSymbols);
  FreeAndNil(fcDefinedOptions);

  inherited;
end;

procedure TConditionalCompilationProcessing.AddDefinedSymbol(const psSymbol: string);
begin
  if (psSymbol <> '') and (not SymbolIsDefined(psSymbol)) then
    fcDefinedSymbols.Add(psSymbol);
end;

procedure TConditionalCompilationProcessing.RemoveDefinedSymbol(const psSymbol: string);
var
  liIndex: integer;
begin
  liIndex := fcDefinedSymbols.IndexOf(psSymbol);
  if liIndex >= 0 then
    fcDefinedSymbols.Delete(liIndex);
end;

function TConditionalCompilationProcessing.SymbolIsDefined(const psSymbol: string): boolean;
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

{
procedure TConditionalCompilationProcessing.AddDefinedOption(const psOption: string);
var
  lsInverseOption: string;
  liIndex: integer;
begin
  if (psOption <> '') and (not OptionIsDefined(psOption)) then
  begin
    fcDefinedSymbols.Add(psOption);

    lsInverseOption := InverseOption(psOption);
    liIndex := fcDefinedOptions.IndexOf(lsInverseOption);
    if liIndex >= 0 then
      fcDefinedOptions.Delete(liIndex);
  end;
end;
}

function TConditionalCompilationProcessing.OptionIsDefined(const psOption: string): boolean;
begin
  Result := fcDefinedOptions.IndexOf(psOption) >= 0;
end;

function TConditionalCompilationProcessing.BlockStartIsIncluded(const pcToken: TSourceToken): Boolean;
var
  leBlockCond: TBlockCondition;
  lsSymbol: string;
begin
  Assert(pcToken <> nil);
  Result := False;

  if GetConditionalBlockType(pcToken) = btBlockStart then
  begin
    leBlockCond := GetBlockCondition(pcToken);
    lsSymbol := GetConditionSymbol(pcToken);

    case leBlockCond of
      bcIfDef:
        Result := SymbolIsDefined(lsSymbol);
      bcIfNotDef:
        Result := not SymbolIsDefined(lsSymbol);
      bcIfOpt:
        Result := OptionIsDefined(lsSymbol);
      else
        Assert(false);
    end;
  end;
end;



{ scan from the else to the end, and hide it all! }
function TConditionalCompilationProcessing.RemoveBlock(
  const piIndex, piStartNestingLevel: integer; const pbStopAtElse: Boolean): boolean;
var
  liLoop: integer;
  liEndIndex: integer;
  liCurrentNestingLevel: integer;
  leType: TBlockMarkerType;
  lcToken: TSourceToken;
  lcStoredItems: TSourceToken;
begin
  Result := False;
  liEndIndex := -1;
  liCurrentNestingLevel := piStartNestingLevel;

  for liLoop := piIndex + 1 to fcTokenList.Count - 1 do
  begin
    lcToken := fcTokenList.SourceTokens[liLoop];

    leType := GetConditionalblockType(lcToken);
    case leType of
    btBlockStart:
      inc(liCurrentNestingLevel);
    btBlockElse:
    begin
      if pbStopAtElse and (liCurrentNestingLevel = piStartNestingLevel) then
      begin
        liEndIndex := liLoop;
        break;
      end;
    end;
    btBlockEnd:
    begin
      if liCurrentNestingLevel = piStartNestingLevel then
      begin
        liEndIndex := liLoop;
        break;
      end;
      dec(liCurrentNestingLevel);
    end
    else
      // no change, do nothing
    end;
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


{ TConditionalCompilationProcessing }

procedure TConditionalCompilationProcessing.ProcessTokenList;
var
  liLoop: integer;
  lcToken: TSourceToken;
  leType: TBlockMarkerType;
begin
  Assert(TokenList <> nil);

  liLoop := 0;
  while liLoop < TokenList.Count - 1 do
  begin
    lcToken := TokenList.SourceTokens[liLoop];

    if lcToken.CommentStyle = eCompilerDirective then
    begin
      if StrHasPrefix(lcToken.SourceCode, ['{$DEFINE']) then
        AddDefinedSymbol(GetConditionSymbol(lcToken))
      else if StrHasPrefix(lcToken.SourceCode, ['{$UNDEF']) then
        RemoveDefinedSymbol(GetConditionSymbol(lcToken))
      else
      begin
        leType := GetConditionalblockType(lcToken);
        case leType of
        btBlockStart:
        begin
          inc(fiStartNestingLevel);
          if not BlockStartIsIncluded(lcToken) then
          begin
            RemoveBlock(liLoop, fiStartNestingLevel, True);
            //dec(fiStartNestingLevel);
         end;
        end;
        btBlockElse:
          if RemoveBlock(liLoop, fiStartNestingLevel, False) then
            dec(fiStartNestingLevel);

        btBlockEnd:
          dec(fiStartNestingLevel);
        else
          // no change, do nothing
        end;
      end;
    end;

    inc(liLoop);
  end;

end;

end.
