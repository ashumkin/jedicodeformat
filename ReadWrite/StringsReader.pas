unit StringsReader;

{
  AFS 1 Jan 2003
  Attach the formatter to TStrings
}

interface

uses
  { delphi } Classes,
  { local } CodeReader;

type
  TStringsReader = class(TCodeReader)
  private
    { property implementation }
    FcInputStrings: TStrings;


  protected
    procedure ReadFromSource; override;
  public
    procedure Clear; override;

    property InputStrings: TStrings read FcInputStrings write FcInputStrings;
  end;


implementation

{ TSTringsReader }

procedure TStringsReader.Clear;
begin
  inherited;
  FcInputStrings := nil;
end;

procedure TStringsReader.ReadFromSource;
begin
  if fbHasRead then
    exit;

  // Open the file
  Assert((FcInputStrings <> nil), 'No source strings');

  fsSource := FcInputStrings.Text;

  fiSourceLength := Length(fsSource);

  fiReadIndex    := 1;
  fiBufferLength := 1;
  fbHasRead      := True;
end;

end.
