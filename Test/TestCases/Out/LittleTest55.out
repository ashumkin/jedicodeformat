unit LittleTest55;

interface

implementation

procedure Foo;
var
  i: integer;
begin
{$IF Defined(LINUX)}
   i := 42;
{$ELSEIF Defined(MSWINDOWS)}
  i := 23;
{$ELSE}
   {$MESSAGE ERROR 'foo not implemented'}
{$IFEND}
end;


end.
