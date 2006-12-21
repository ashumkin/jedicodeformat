unit TestDelphiNetAmpersandMethod;

interface

type
  TClass1 = class
  private
    EmptyValue: System.&Object;
  public

    function TestFile: Boolean;
    constructor Create;
  end;

var
  AType: &Type;
  AnotherType: System.&Type;
  AnObject: &Object;
  AnotherObject: System.&Object;

implementation

uses
  System.Net,
  System.IO;

constructor TClass1.Create;
var
  weby1, weby2: WebRequest;
begin
  inherited Create;

  { this is *not* the constructor
     the ampersand signals that it is a CLR method 
  }
  weby1 := WebRequest.&Create('http://www.google.com');
  weby2 := System.Net.WebRequest.&Create('http://www.google.com');
end;

function TClass1.TestFile: Boolean;
begin
  result := System.IO.&File.Exists('XY');
end;

end.
