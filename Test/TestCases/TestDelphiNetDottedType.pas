unit TestDelphiNetDottedType;

interface

type

TMyClass = class
public
  pElement: System.Object;
end;


TMyClass2 = class
public
  function GetObject(MyType: System.Type): TObject;
end;

implementation

{ TMyClass2 }

function TMyClass2.GetObject(MyType: System.Type): TObject;
begin
  Result := nil;
end;

end.
