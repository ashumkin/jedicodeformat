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
begin
  if fcToken = nil then
    Result := ''
  else
  begin
    Result := fcToken.Describe;
    if fcToken.YPosition > 0 then
    begin
      Result := Result + ' on line ' + IntToStr(fcToken.YPosition);
      if fcToken.XPosition > 0 then
        Result := Result + ' position ' + IntToStr(fcToken.XPosition);
    end;
  end;

end;

end.
