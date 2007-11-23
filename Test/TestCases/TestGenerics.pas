unit TestGenerics;


{ AFS November 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}


interface

uses
  SysUtils;

type
  TGenericType<AnyType> = class
    FData: AnyType;
    function GetData: AnyType;
    procedure SetData(Value: AnyType);
    property Data: AnyType read GetData write SetData;
  end;

implementation

function TGenericType<AnyType>.GetData: AnyType;
begin
  Result := FData;
end;

procedure TGenericType<AnyType>.SetData(Value: AnyType);
begin
  FData := Value;
end;

type
  TGenericTypeInt = TGenericType<Integer>;

var
  I: TGenericTypeInt;

initialization
 try
  With TGenericType<Integer>.Create do
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