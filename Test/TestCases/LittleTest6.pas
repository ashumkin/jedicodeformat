unit LittleTest6;

interface

function Bar: integer;

implementation

function Foo: integer;
begin
  Result := 3;
end;

function Bar: integer;
begin
  Result := Foo() + 3;
end;

end.
