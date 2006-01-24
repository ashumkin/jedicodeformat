unit TestDelphiNetDottedType;

interface

type

TMyClass = class
public
    MSExcelType: System.Type;
    ObjValue : System.Object;
end;


TMyClass2 = class
public
  function GetObject(MyType: System.Type): TObject;
end;

implementation

{ TMyClass2 }

function TMyClass2.GetObject(MyType: System.Type): TObject;
begin
  ObjValue := System.Object(Convert.ToDouble(Console.ReadLine));
  MSExcelType := System.Type.GetTypeFromProgID('Excel.Application', True);  

  Result := nil;
end;

end.
