unit TestGenerics2;


{ AFS November 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - like TestGenerics, but all generics have 2 params
}


interface

uses
  SysUtils;

type
  TGenericType<AnyType1, AnyType2> = class
    FData: AnyType;
    function GetData: AnyType;
    procedure SetData(Value: AnyType);
    property Data: AnyType read GetData write SetData;
  end;

implementation

function TGenericType<AnyType1, AnyType2>.GetData: AnyType;
begin
  Result := FData;
end;

procedure TGenericType<AnyType1, AnyType2>.SetData(Value: AnyType);
begin
  FData := Value;
end;

type
  TGenericTypeInt = TGenericType<Integer, Integer>;

var
  I: TGenericTypeInt;

initialization
 try
  With TGenericType<Integer, Integer>.Create do
   Data := 100;

  I := TGenericTypeInt.Create;
  I.Data := 100;
  WriteLn(I.Data);

  ReadLn;
 except
  on E:Exception do
   Writeln(E.Classname, ': ', E.Message);
 end;
end.