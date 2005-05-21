unit TestDelphiNetOperatorOverload;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

  test operator overloads in Delphi.NET }

interface

type
  TMyClass = class
  private
    FData: Integer;
  public
    class operator Add(A,B: TMyClass): TMyClass;
    class operator Implicit(A: Integer): TMyClass;
    class operator Implicit(A: TMyClass): Integer;

    property Data: Integer read FData write FData;
  end;

implementation

{ TMyClass }

class operator TMyClass.Add(A, B: TMyClass): TMyClass;
begin
  Result := TMyClass.Create;
  Result.Data := A.Data + B.Data;
end;

class operator TMyClass.Implicit(A: TMyClass): Integer;
begin
  Result := A.Data;
end;

class operator TMyClass.Implicit(A: Integer): TMyClass;
begin
  Result := TMyClass.Create;
  Result.Data := A;
end;

end.
