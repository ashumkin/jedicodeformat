unit TestDelphi2009Inherited;

{ AFS November 2008
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 This code test Delphi 2009 Features  }

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
