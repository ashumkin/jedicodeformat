unit TestHexConstantElse;

interface

implementation

procedure TestHexConstantElseProc;
var
  v: integer;
begin

if 1 > 2 then
v := $000080FF
else
v := $0000FF80;

end;

end.
