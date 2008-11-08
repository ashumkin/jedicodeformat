unit TestD9Inherited;

interface

type
  TBase = class
  public
    function Foo: integer; virtual;
  end;

  TDerived = class(TBase)
  public
    function Foo: integer; override;
  end;

  TDerived2 = class(TBase)
  public
    function Foo: integer; override;
  end;

implementation

{ TBase }

function TBase.Foo: integer;
begin
  Result := 42;
end;

{ TDerived }

function TDerived.Foo: integer;
begin
  Result := inherited;
end;

{ TDerived2 }

function TDerived2.Foo: integer;
begin
  Result := inherited + 2;
end;

end.
