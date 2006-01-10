unit TestDelphiNetAmpersandMethod;

interface

type
  TClass1 = class
  private
    { Private Declarations }
  public
    constructor Create;
  end;

implementation

uses
  System.Net;

constructor TClass1.Create;
var
  weby1, weby2: WebRequest;
begin
  inherited Create;

  { this is *not* the constructor
     the ampersand signals this 
  }
  weby1 := WebRequest.&Create('http://www.google.com');
  weby2 := System.Net.WebRequest.&Create('http://www.google.com');
end;

end.
