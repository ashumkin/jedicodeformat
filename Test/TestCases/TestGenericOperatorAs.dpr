program TestGenericOperatorAs;

{$APPTYPE CONSOLE}


{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type
 MaClasse=class
  Function  Contraint<X:Class>(AValue: TObject):X;
  Function  NonContraint<X>(AValue: TObject):X;
  procedure Test;
 end;

Function MaClasse.Contraint<X>(AValue: TObject):X;
begin
 Result:=AValue as X;
end;

Function MaClasse.NonContraint<X>(AValue: TObject):X;
begin
 Result:=AValue as X;
end;


procedure MaClasse.Test;
Var Resultat : TObject;
    Str :String;
    Int:Integer;
begin
 Resultat:=TObject.Create;
 Writeln('Classe de Resultat = ',Resultat.ClassName);
 Resultat:=Contraint<TObject>(Str); // Si Nil, Resulat = Nil
 Resultat:=NonContraint<TObject>(Str); // Si Nil, Resulat = Nil
 Writeln('Resultat = Nil');
 try
  //Int:=Contraint<Integer>(Str);  //E2511 Le paramètre type 'X' doit être un type de classe
  Int:=NonContraint<Integer>(Str);  //NullReferenceException:
 except
   on E:NullReferenceException do;
 end;

 Str:='Test'; //Intanciation de Str
 Resultat:=TObject.Create;
 Writeln('Classe de Resultat = ',Resultat.ClassName); //Resultat = TObject
 Resultat:=Contraint<TObject>(Str); //Ok
 Writeln('Classe de Resultat = ',Resultat.ClassName); //Resultat = TString;
 Resultat:=NonContraint<TObject>(Str); //Ok

 try
   Int:=NonContraint<Integer>(Str); //InvalidCastException
 except
   on E:InvalidCastException do;
 end;

end;


var Classe : MaClasse;

begin
  Classe:=MaClasse.Create;
 try
  Classe.Test;
 except
   on E:Exception do
     Writeln(E.Classname, ': ', E.Message);
 end;
  readln;
end.

