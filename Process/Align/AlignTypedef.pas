unit AlignTypedef;

interface

uses SourceToken, AlignBase;

type

  TAlignTypedef = class(TAlignBase)
  protected
    { TokenProcessor overrides }
    function IsTokenInContext(const pt: TSourceToken): boolean; override;

      { AlignStatements overrides }
    function TokenIsAligned(const pt: TSourceToken): boolean; override;
    function TokenEndsStatement(const pt: TSourceToken): boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses FormatFlags, JcfSettings, ParseTreeNodeType, WordMap, TokenType;

constructor TAlignTypedef.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignTypedef];
end;

function TAlignTypedef.IsIncludedInSettings: boolean;
begin
  Result := (not Settings.Obfuscate.Enabled) and Settings.Align.AlignTypedef;
end;

function TAlignTypedef.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := pt.HasParentNode(nTypeDecl) and (not pt.HasParentNode(ObjectTypes)) and
    ((not Settings.Align.InterfaceOnly) or (pt.HasParentNode(nInterfaceSection)));
end;

function TAlignTypedef.TokenEndsStatement(const pt: TSourceToken): boolean;
begin
  { only look at solid tokens }
  if (pt.TokenType in [ttReturn, ttWhiteSpace]) then
  begin
    Result := False;
  end
  else
  begin
    Result := (not pt.HasParentNode(nTypeDecl)) or
      (pt.TokenType in [ttSemiColon, ttEOF]) or pt.HasParentNode(ObjectTypes + [nRecordType]);

    // ended by a blank line
    if (pt.TokenType = ttReturn) and (pt.SolidTokenOnLineIndex <= 1) then
      Result := True;
  end;
end;

function TAlignTypedef.TokenIsAligned(const pt: TSourceToken): boolean;
begin
  Result := (pt.Word = wEquals);
end;

end.
