unit LittleTest23;

interface

implementation

var
  Bar: Boolean;

function Foo(const Chr: Byte): Boolean;
begin
  Result := (Bar) and (Chr in [$B2..$EC]);
end;

end.
