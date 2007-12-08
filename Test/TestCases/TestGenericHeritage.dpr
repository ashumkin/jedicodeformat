program TestGenericHeritage;

{$APPTYPE CONSOLE}


{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;
type
   //Classe de base
  TClassDeBase=Class
   FData: integer;
  End;

   //Classe générique
  TGenerique<I>=Class
   FData: I;
  End;

   //Classe de base dérivée d'un type construit ouvert
  {TClassDeBaseDeriveDeGenerique=Class(TGenerique<I>) //E2003 Identificateur non déclaré : 'I'
   FData: integer;
  End;
  }
   //Classe de base dérivée d'un type construit fermé
  TClassDeBaseDeriveDeGenerique=Class(TGenerique<Integer>)
   FData: integer;
  End;

   //Classe générique dérivée d'une classe de base
  TGeneriqueDeriveeDeClass<I>=Class(TClassDeBase)
   FData: I;
  End;

   //Classe générique dérivée d'une classe générique
  TGeneriqueDeriveeDeGenerique<I,S>=Class(TGeneriqueDeriveeDeClass<I>)
   FData: I;
   Fitem: S;
  End;

   //Classe générique contrainte
  TGeneriqueContraint<I:constructor>=Class
   FData: I;
  End;

  //Classe générique dérivée d'une classe générique contrainte
   //Dans ce cas les contraintes doivent être redéclarées.
  TGeneriqueDeriveeDeGeneriqueContraint<I:constructor,S>=Class(TGeneriqueContraint<I>)
   FData: I;
   Fitem: S;
  End;


  TParent<T> = class
    X: T;
  end;

  TEnfant<S> = class(TParent<S>)
    //Y: T;  // Erreur! Identifieur inconnu "T"
    Y: S;
  end;


var
  F: TParent<Integer>;
begin
  try
   F:=TParent<Integer>.Create;
   F.X:=10;  // Erreur! Identifieur inconnu "T"
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
