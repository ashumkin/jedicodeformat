unit TestClassMethods;

{ AFS 15 July 2007

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  this unit tests class methods and properties  }

interface

type
  TFooForm = class
  private
    var foo: integer;
    class var fish: integer;
    class function GetWibble: integer; static;
  public
    class function Foo: boolean;
    class procedure Bar;

    class property Wibble: integer read GetWibble;
  end;


implementation

{ TFooForm }

class procedure TFooForm.Bar;
begin
  inc(fish);
end;

class function TFooForm.Foo: boolean;
begin
  Result := true;
end;

class function TFooForm.GetWibble: integer;
begin
  Result := fish;
end;

end.
