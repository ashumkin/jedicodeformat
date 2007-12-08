program TestGenericFunctions;

{$APPTYPE CONSOLE}


{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type
  TProcedureGenerique<A> = procedure(Param1 : A);
  TProcObjetGenerique<B> = procedure(X,Y: B) of object;
  TFonctionGenerique<T>  = Function : T;

  TMaClasse = class
    procedure UneMethode<T>(X, Y: T); // Même signature que le type TProcObjetGenerique<B>
    procedure TestMethode;
    procedure TestProcedure<UnType>(Prc:TProcedureGenerique<UnType>);
    procedure TestFonction<T>(fnct: TFonctionGenerique<T>);
  end;

//Procedure ProcedureGenerique<A>(M:A); //E2530 Les paramètres de type ne sont pas autorisés sur la fonction ou la procédure globale
Procedure ProcedureGeneriqueInt(M:Integer);
begin
  Writeln(M);
end;

Procedure ProcedureGeneriqueString(M:String);
begin
  Writeln(M);
end;

function UneFonctionGeneriqueInt: Integer;
var S : String;
begin
  Result := 10;
end;

{On ne peux pas utiliser de type construit ouvert dans un type procedure
procedure TestTypeProcedure<A>(Arg:TMaClasse); -> E2530 Les paramètres de type ne sont pas autorisés sur la fonction ou la procédure globale
}
procedure TestTypeProcedure(Arg:TMaClasse);
var P:TProcObjetGenerique<String>;
    P2:TProcObjetGenerique<Integer>;
    //Pn : TProcObjetGenerique<...
begin
  P:=Arg.UneMethode<String>;
  P('Hello', 'World');

  //P:=Arg.UneMethode<Integer>;   //E2010 Types incompatibles : 'string' et 'Integer'
  P2:=Arg.UneMethode<Integer>;
  P2(10, 20);

end;

 {TMaClasse}
procedure TMaClasse.UneMethode<T>(X, Y: T);
begin
  Writeln(X.ToString,' , ',Y.ToString);
end;

procedure TMaClasse.TestMethode;
var
  P: TProcObjetGenerique<Boolean>;
begin
   //On détermine le type dans le corps de la méthode. La signature est donc connue.
  UneMethode<String>('Hello', 'World');
  UneMethode('Hello', 'World');

  UneMethode<Integer>(10, 20);
  UneMethode(10, 20);

  P:=UneMethode<Boolean>;
  P(False,True);
end;

procedure TMaClasse.TestProcedure<UnType>(Prc:TProcedureGenerique<UnType>);
var
  P: TProcedureGenerique<UnType>;
  Value: UnType;
begin
   //Le type est déterminé dans la signature, il est donc inconnu.
  Prc('Hello');
  P:=Prc;
  //Value:='Chaine'; //E2010 Types incompatibles : 'UnType' et 'string'
  Value:=Default(UnType);

  if assigned(TObject(Value)) //Le cast est obligatoire
   then writeln('Assigné.')
   else writeln('Non assigné.');
  P(Default(UnType));

    //Todo
  //Prc(True); //Compile mais provoque une exception. Le compilo devrais controler le type de l'agument ?
  //P(True);
end;

procedure TMaClasse.TestFonction<T>(fnct: TFonctionGenerique<T>);
Var I : T;
begin
 I:=Fnct;
 Writeln('Retour de fonction = ',TObject(I));
end;

var MaClasse : TMaClasse;
begin
 try
  MaClasse:=TMaClasse.Create;
  With MaClasse do
  begin
   TestTypeProcedure(MaClasse);
   TestMethode;
   TestProcedure<Integer>(ProcedureGeneriqueInt);

   //TestProcedure<Integer>(ProcedureGeneriqueString); //E2010 Types incompatibles : 'Integer' et 'string'
   //TestProcedure<String>(ProcedureGeneriqueInt); //E2010 Types incompatibles : 'string' et 'Integer'
   try
    TestProcedure<String>(ProcedureGeneriqueString);
   except
    on E:Exception do
     Writeln(E.Classname, ': ', E.Message);
   end;
   TestFonction<Integer>(UneFonctionGeneriqueInt);
  end;

 except
  on E:Exception do
    Writeln(E.Classname, ': ', E.Message);
 end;
 Readln;
end.
