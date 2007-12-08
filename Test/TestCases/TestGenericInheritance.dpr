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
  //Surcharge g�n�rique
  TTest<I>=Class
   FData: I;
  End;

  {La surcharge bas�e sur une contrainte est impossible.
  TTest<I:constructor>=Class //E2037 La d�claration de 'TTest<I>' diff�re de la d�claration pr�c�dente
   FData: I;
  End;
  }

  TTest<I,S>=Class
   FData: I;
   Fitem: S;
  End;

  //Surcharge g�n�rique � partir d'une autre surcharge g�n�rique de la m�me classe
      TGeneric<A,B>=Class
       FData: A;
       Fitem: B;
      End;

  TGeneric<A>=Class(TGeneric<A,String>)
   FData: A;
  End;

  //Surcharge g�n�rique � partir d'une autre surcharge g�n�rique d'une autre classe
  TGenericTest<R,U>=Class(TGeneric<R>)
   FData: R;
  End;

  {La surcharge bas�e sur une type valeur au lieu d'un type r�f�rence est impossible.
   TGenericTest<R,U>=Record //E2004 Identificateur red�clar� : 'TGenericTest'

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
