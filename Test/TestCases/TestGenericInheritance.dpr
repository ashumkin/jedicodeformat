program TestGenericInheritahnce;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;
type

  TTest=Class
   FData: integer;
  End;
  //Surcharge générique
  TTest<I>=Class
   FData: I;
  End;

  {La surcharge basée sur une contrainte est impossible.
  TTest<I:constructor>=Class //E2037 La déclaration de 'TTest<I>' diffère de la déclaration précédente
   FData: I;
  End;
  }

  TTest<I,S>=Class
   FData: I;
   Fitem: S;
  End;

  //Surcharge générique à partir d'une autre surcharge générique de la même classe
      TGeneric<A,B>=Class
       FData: A;
       Fitem: B;
      End;

  TGeneric<A>=Class(TGeneric<A,String>)
   FData: A;
  End;

  //Surcharge générique à partir d'une autre surcharge générique d'une autre classe
  TGenericTest<R,U>=Class(TGeneric<R>)
   FData: R;
  End;

  {La surcharge basée sur une type valeur au lieu d'un type référence est impossible.
   TGenericTest<R,U>=Record //E2004 Identificateur redéclaré : 'TGenericTest'

  End;
  }
var Gen1 : TTest;
    Gen2 : TTest<Integer>;
    Gen3 : TTest<Integer,String>;
begin
  try
   Gen1:= TTest.Create;
   Gen2:= TTest<Integer>.Create;
   Gen3:= TTest<Integer,String>.Create;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
