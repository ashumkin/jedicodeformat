unit ParseError;

interface

uses
  {delphi }
  SysUtils,
  { local }
  SourceToken;

type
  TEParseError = class(Exception)
  private
    fcToken: TSourceToken;
    function GetTokenMessage: string;

  public
    Constructor Create(const psMessage: string; const pcToken: TSourceToken);

    property TokenMessage: string read GetTokenMessage;
  end;

implementation

{ TEParseError }

constructor TEParseError.Create(const psMessage: string; const pcToken: TSourceToken);
begin
  inherited Create(psMessage);

  fcToken := pcToken;
end;

function TEParseError.GetTokenMessage: string;
var
  lsPos: string;
begin
  if fcToken = nil then
    Result := ''
  else
  begin
    Result := fcToken.Describe;
    lsPos := fcToken.DescribePosition;
    if lsPos <> '' then
      Result := Result + ' ' + lsPos;
  end;

end;

end.
