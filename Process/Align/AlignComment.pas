unit AlignComment;

interface

uses SourceToken, AlignBase;

type
  TAlignComment = class(TAlignBase)
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

uses FormatFlags, JcfSettings, ParseTreeNodeType, TokenType, TokenUtils;

constructor TAlignComment.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignComment];
end;

function TAlignComment.IsIncludedInSettings: boolean;
begin
  Result := (not Settings.Obfuscate.Enabled) and Settings.Align.AlignComment;
end;

function TAlignComment.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := (not Settings.Align.InterfaceOnly) or (pt.HasParentNode(nInterfaceSection));
end;

function TAlignComment.TokenEndsStatement(const pt: TSourceToken): boolean;
begin
  { only look at solid tokens and returns }
  if (pt.TokenType in [ttWhiteSpace]) then
  begin
    Result := False;
  end
  else
  begin
    Result := (pt.TokenType in [ttReturn, ttEOF]);
  end;
end;

function TAlignComment.TokenIsAligned(const pt: TSourceToken): boolean;
var
  ltNext: TSourceToken;
begin
  // must be a comment on one line
  Result := IsSingleLineComment(pt);
  if Result then
  begin
    // must be the last thing on the line
    ltNext := pt.NextTokenWithExclusions([ttWhitespace]);
    Result := (ltNext <> nil) and (ltNext.TokenType = ttReturn);
  end;

end;

end.
