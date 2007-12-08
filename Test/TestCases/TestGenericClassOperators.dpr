program TestGenericClassOperators;

{$APPTYPE CONSOLE}


{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}


uses
  SysUtils;

type
  //Ok
 TOperateur<T> = class
    Champ1: T;
    Procedure Test(const Value: TOperateur<T>);
    class operator Negative(const Value: TOperateur<T>): TOperateur<T>;
 end;
  //Ok
 TMonRecord1 = record
  Procedure Test(const Value: TMonRecord1);
  class operator Negative(const Value: TMonRecord1): TMonRecord1;
 end;

   //NOk
 TMonRecord<T> = record
    Champ1: T;
    Procedure Test(const Value: TMonRecord<T>);  //E2086 Le type 'TMonRecord<T>' n'est pas encore complètement défini
     //Opérateur Unaire -
    class operator Negative(const Value: TMonRecord<T>): TMonRecord<T>;
 end;

end.
