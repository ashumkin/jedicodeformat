unit StringsWriter;

{
  Write converter output to strings
}

interface

uses
  { delphi } Classes,
  { local } CodeWriter;

type
  TStringsWriter = class(TCodeWriter)
  private
    { properties }
    fcOutputStrings: TStrings;

  protected

  public
    constructor Create; override;
    procedure Close;  override;

    property OutputStrings: TStrings read fcOutputStrings write fcOutputStrings;
  end;

implementation

{ TStringsWriter }
constructor TStringsWriter.Create;
begin
  inherited;
  fcOutputStrings := nil;
end;

procedure TStringsWriter.Close;
begin
  if BOF then
    exit;

  Assert(fcOutputStrings <> nil);

  BeforeWrite;
  fcOutputStrings.Text := fsDestText;
end;


end.
