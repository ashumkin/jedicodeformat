program TestGenericOperatorIs;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils,
  classes;

type
 MaClasse<T>=class
  unchamp:T;
  procedure Test<X>;
  procedure Test2<X>;
  procedure Test3<X:constructor>;
 end;

 NullInteger = System.Nullable<Integer>;

procedure MaClasse<T>.Test<X>;
var Variable:X;
begin
 Writeln;
 Write(#9+'Le test sur X<',Typeof(X),'> is T<',typeof(T),'> est ' );
 if TObject(Variable) is T
  then writeln('Vrai')
  else writeln('Faux');
  if assigned(TObject(Variable))
   then writeln(#9+'assigned(Variable) =Vrai')
   else writeln(#9+'assigned(Variable)=Faux');
end;

procedure MaClasse<T>.Test2<X>;
var Variable:X;
begin
{ ne change rien au test}
 if assigned(TObject(Variable))
   then Variable:=Default(X);
 Writeln;
 Write(#9+'Le test sur X<',Typeof(X),'> is T<',typeof(T),'> est ' );
 if TObject(Variable) is T
  then writeln('Vrai')
  else writeln('Faux');
end;

procedure MaClasse<T>.Test3<X>;
var Variable:X;
begin
 Writeln;
 Write(#9+'Le test sur X<',Typeof(X),'> is T<',typeof(T),'> est ' );
 Variable:=X.Create;
 if TObject(Variable) is T
  then writeln('Vrai')
  else writeln('Faux');
  if assigned(TObject(Variable))
   then writeln(#9+'assigned(Variable) =Vrai')
   else writeln(#9+'assigned(Variable)=Faux');
end;


procedure Test(Resultat:Boolean);
begin
 if Resultat
  then writeln('Vrai')
  else writeln('Faux');
end;

var Classe1: MaClasse<TObject>;
    Classe2: MaClasse<System.Nullable<Integer>>;
    Classe3: MaClasse<Integer>;
    VarNull: NullInteger;
    Objet  : TObject;
    int    : Integer;


begin
 try
   //Les commentaires en anglais proviennent des spec du C# 2.0
  //If the type of e is a reference type or a nullable type and the value of e is null, the result is false.
  // Sous Delphi ce test est toujours vrai
  Writeln('Test pour la variable varNull = null');
  varNull:=Default(System.Nullable<Integer>);
  Test(varNull is  NullInteger);
  varNull:=10;
  Test(varNull is  NullInteger);


  Writeln('Test pour la variable Objet à nil'); //Comportement identique à Delphi Win32
  Test(Objet is TObject);
{Otherwise, let D represent the dynamic type of e as follows :}
  //If the type of e is a reference type, D is the run-time type of the instance reference by e.
  Objet:=TObject.Create;
  Writeln('Test pour la variable Objet instanciée');
  Test (Objet is TObject);

  //If the type of e is a nullable type, D is the underlying type of that nullable type.
  varNull:=10;
  Writeln('Test pour la variable varNull = 10');
  Test (varNull is System.Nullable<Integer>);

  //If the type of e is a non-nullable value type, D is the type of e.
  Int:=25;
  Writeln('Test pour la variable int');
  Test (Int is Integer);
  //Test (varnull is Integer);    //E2010 Types incompatibles : 'Nullable<System.Int32>' et 'Integer'
  //Test (int is System.Nullable<Integer>);   //E2010 Types incompatibles : 'Integer' et 'Nullable<System.Int32>'

{The result of the operation depends on D and T as follows:}
  //If T is a reference type, the result is true if D and T are the same type,
  //if D is a reference type and an implicit reference conversion from D to T exists,
  //or if D is a value type and a boxing conversion from D to T exists.
 Classe1:=MaClasse<TObject>.Create;
 Writeln('Test pour la variable Classe1 MaClasse<TObject>');
 Classe1.Test<Integer>;
 //Classe1.Test3<Integer>;
 Classe1.Test<TObject>;
 Classe1.Test3<TObject>;
 Classe1.Test3<TList>;
 Classe1.Test3<integer>;
 Writeln;

 //If T is a nullable type, the result is true if D is the underlying type of T.
 Writeln('Test pour la variable Classe2 MaClasse<System.Nullable<Integer>>');
 Classe2:=MaClasse<NullInteger>.Create;
 Classe2.Test<Double>;
 Classe2.Test<String>;
 Classe2.Test<integer>;
 Classe2.Test<NullInteger>;

 //If T is a non-nullable value type, the result is true if D and T are the same type.
 //Otherwise, the result is false.
 Writeln('Test pour la variable Classe3 MaClasse<Integer>');
 Classe3:=MaClasse<Integer>.Create;
 Classe3.Test<Double>;
 Classe3.Test<integer>;
 Classe3.Test<String>;
 Classe3.Test<NullInteger>;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;

end.
