program TestGenericInterface;

{$APPTYPE CONSOLE}


{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;
type
  IMonInterface<T>= interface
    procedure set_Valeur(const AValeur: T);
    function get_Valeur: T;
    property Valeur: T read get_Valeur write set_Valeur;
  end;

  IMonInterfaceDerivee<T>= interface(IMonInterface<T>)
   Procedure Multiplier(AMulplicateur:T);
  end;

  TClasseTest<T> = class(TObject,IMonInterfaceDerivee<T>)
  private
    FCompteur: T;
  public
    procedure set_Valeur(const AValeur: T);
    function get_Valeur: T;
    Procedure Multiplier(AMulplicateur:T);
    Procedure FaitQuelQueChose;
  end;

{TClasseTest<T>}
procedure TClasseTest<T>.set_Valeur(const AValeur: T);
begin
 FCompteur:=AValeur;
end;

function TClasseTest<T>.get_Valeur: T;
begin
  Result:=FCompteur;
end;


Procedure TClasseTest<T>.Multiplier(AMulplicateur:T);
begin
 // On ne connait rien du type T on ne peut donc utiliser l'opérateur Multiplier
 //FCompteur:=FCompteur * AMulplicateur;  //E2015 Opérateur non applicable à ce type d'opérande
end;

Procedure TClasseTest<T>.FaitQuelQueChose;
begin
  Writeln(TypeOf(T));
end;

var Obj:TClasseTest<Integer>;
    Obj2:TClasseTest<Double>;

begin
  try
   Obj:=TClasseTest<Integer>.Create;
   Obj.Set_Valeur(10);
   Writeln(Obj.FCompteur);
   Obj.FaitQuelQueChose;
Writeln;
   Obj2:=TClasseTest<Double>.Create;
   Obj2.Set_Valeur(10);
   Writeln(Obj2.FCompteur);
   Obj2.FaitQuelQueChose;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.
