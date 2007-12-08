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

   //Classe g�n�rique
  TGenerique<I>=Class
   FData: I;
  End;

   //Classe de base d�riv�e d'un type construit ouvert
  {TClassDeBaseDeriveDeGenerique=Class(TGenerique<I>) //E2003 Identificateur non d�clar� : 'I'
   FData: integer;
  End;
  }
   //Classe de base d�riv�e d'un type construit ferm�
  TClassDeBaseDeriveDeGenerique=Class(TGenerique<Integer>)
   FData: integer;
  End;

   //Classe g�n�rique d�riv�e d'une classe de base
  TGeneriqueDeriveeDeClass<I>=Class(TClassDeBase)
   FData: I;
  End;

   //Classe g�n�rique d�riv�e d'une classe g�n�rique
  TGeneriqueDeriveeDeGenerique<I,S>=Class(TGeneriqueDeriveeDeClass<I>)
   FData: I;
   Fitem: S;
  End;

   //Classe g�n�rique contrainte
  TGeneriqueContraint<I:constructor>=Class
   FData: I;
  End;

  //Classe g�n�rique d�riv�e d'une classe g�n�rique contrainte
   //Dans ce cas les contraintes doivent �tre red�clar�es.
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
