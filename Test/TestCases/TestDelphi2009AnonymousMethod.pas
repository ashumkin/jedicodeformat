unit TestDelphi2009AnonymousMethod;

{ AFS November 2008
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 This code test Delphi 2009 Features  }

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Classes;

type
  TIntProcedure = reference to procedure (var x: Integer);
  TIntFunction = reference to function (var x: Integer): integer;
  TGenericFunction<T> = reference to function(var x: T): T;

implementation

procedure SortTest;
var
  liList: TList<integer>;
begin
  liList := TList<integer>.Create();

  liList.Add(48);
  liList.Add(12);
  liList.Add(400);
  liList.Add(24);
  liList.Add(4);

  liList.Sort(TComparer<integer>.Construct(
     function(const Item1, Item2: integer): Integer
      begin
         Result := Item1 - Item2;
       end));
end;

procedure StreamTest;
var
  a: integer;
begin

  while (function (Stream: TStream; var a: Integer; b: Integer): Integer
  begin
    result := 50;
  end)(nil, a, 0) > 0 do
  begin
  end;

end;

function MakeIntProcedure: TIntProcedure;
begin
  Result := procedure (var x: Integer)
    begin
      {whatever}
    end;
end;

function MakeIntFunction: TIntFunction;
begin
  Result := function (var x: Integer): integer
    begin
      Result := x + 1;
    end;
end;


end.
