unit TestDelphiNetDottedType;

interface

type

TMyClass = class
public
  MSExcelType: System.Type;
  ObjValue : System.Object;
end;


TMyClass2 = class
private
  ObjValue : System.Object;
  MSExcelType: System.Type;
public
  function GetObject(MyType: System.Type): TObject;

  procedure CatchDottedException;
end;

implementation

uses
  System.Runtime.Remoting;

{ TMyClass2 }

procedure TMyClass2.CatchDottedException;
begin
  try

  except
   // Parser exception near DO keyword
   on exp: System.Runtime.Remoting.RemotingException do
   begin

   end;

  end;

end;

function TMyClass2.GetObject(MyType: System.Type): TObject;
begin
  ObjValue := System.Object(Convert.ToDouble(Console.ReadLine));
  MSExcelType := System.Type.GetTypeFromProgID('Excel.Application', True);

  Result := nil;
end;

end.
