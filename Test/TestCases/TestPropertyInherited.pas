Unit TestPropertyInherited;

{ bug #762354 in jcf 2 Beta 5
  reported by adem baba
}

Interface

type

TCustomFoo = class(TObject)
  private
    function GetFoo: integer;

  public
    property Foo: integer read GetFoo;
end;

TFoo = Class(TCustomFoo)
 Published
  Property Foo;
End;

Implementation

{ TCustomFoo }

function TCustomFoo.GetFoo: integer;
begin
  Result := 3;
end;

End.