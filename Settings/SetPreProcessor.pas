unit SetPreprocessor;


interface

{ settings for preprocessor
}

uses
  { delphi }
  Classes,
  { local }
  JCFSetBase, SettingsStream;

type
  TSetPreProcessor = class(TSetBase)
  private
    fbEnabled: Boolean;
    fcDefinedSymbols: TStringList;
    fcDefinedOptions: TStringList;

    procedure AddDefaultSymbols;
    procedure AddDefaultOptions;
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Enabled: boolean read fbEnabled write fbEnabled;

    function OptionIsDefined(const psOption: string): boolean;
    function SymbolIsDefined(const psSymbol: string): boolean;

    property DefinedSymbols: TStringList read fcDefinedSymbols write fcDefinedSymbols;
    property DefinedOptions: TStringList read fcDefinedOptions write fcDefinedOptions;

  end;

implementation

uses SysUtils;

const
  REG_ENABLED    = 'Enabled';
  REG_DEFINED_SYMBOLS = 'DefinedSymbols';
  REG_DEFINED_OPTIONS = 'DefinedOptions';

{ TSetPreProcessor }

constructor TSetPreProcessor.Create;
begin
  inherited;
  SetSection('PreProcessor');

  fcDefinedSymbols := TStringList.Create;
  //fcDefinedSymbols.Sorted := True;
  fcDefinedSymbols.Duplicates := dupIgnore;

  fcDefinedOptions := TStringList.Create;
  //fcDefinedOptions.Sorted := True;
  fcDefinedOptions.Duplicates := dupIgnore;
end;

destructor TSetPreProcessor.Destroy;
begin
  FreeAndNil(fcDefinedSymbols);
  FreeAndNil(fcDefinedOptions);
  inherited;
end;

procedure TSetPreProcessor.AddDefaultSymbols;
begin
  fcDefinedSymbols.Add('MSWINDOWS');
  fcDefinedSymbols.Add('WIN32');
  fcDefinedSymbols.Add('DELPHI5_UP');
end;

procedure TSetPreProcessor.AddDefaultOptions;
begin

end;

procedure TSetPreProcessor.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbEnabled := pcStream.Read(REG_ENABLED, True);

  fcDefinedSymbols.Sorted := False;
  if not pcStream.Read(REG_DEFINED_SYMBOLS, fcDefinedSymbols) then
    AddDefaultSymbols;

  fcDefinedSymbols.Sort;
  fcDefinedSymbols.Sorted := True;


  fcDefinedOptions.Sorted := False;
  if not pcStream.Read(REG_DEFINED_OPTIONS, fcDefinedOptions) then
    AddDefaultOptions;

  fcDefinedOptions.Sort;
  fcDefinedOptions.Sorted := True;
end;

procedure TSetPreProcessor.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_DEFINED_SYMBOLS, fcDefinedSymbols);
  pcOut.Write(REG_DEFINED_OPTIONS, fcDefinedOptions);

end;

function TSetPreProcessor.OptionIsDefined(const psOption: string): boolean;
begin
  Result := fcDefinedOptions.IndexOf(psOption) >= 0;
end;

function TSetPreProcessor.SymbolIsDefined(const psSymbol: string): boolean;
begin
  Result := fcDefinedSymbols.IndexOf(psSymbol) >= 0;
end;

end.
