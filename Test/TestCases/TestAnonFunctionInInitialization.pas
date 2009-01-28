unit TestAnonFunctionInInitialization;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
 to test delphi 2009 anon functions }


interface

implementation

Type
  TStringFunction = reference to function: string;

var
  foo: TStringFunction;
  bar: string;

initialization

foo := function: String
  begin
    result := 'fred';
  end;

  bar := foo();

end.

