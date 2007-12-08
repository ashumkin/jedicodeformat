program TestGenericConstructeurStatic;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type
 EContrainteArgumentException=Class(ArgumentException);

 MaClasse<T>=Class
   UnChamp : T;
   S       : String;
   Constructor Create;
   Class Constructor CreateClass;
  end;

Constructor MaClasse<T>.Create;
begin
 inherited;
 S:='Initialisation';
 Writeln(#9+#9+#9+'Appel du constructeur d''instance',Self.Classname, '.Create');
end;

Class Constructor MaClasse<T>.CreateClass;
var UnEntier:Integer;
    VarGeneric :T;
begin
 Writeln;
 Writeln(#9+#9+'Appel du constructeur de classe MaClasse.CreateClass<',Typeof(T),'>');
 VarGeneric:=default(T);
 UnEntier:=10;

  //Est-ce un type valeur et le param�tre de type est-il un integer ?
 if (assigned(TObject(VarGeneric))=true) and (TObject(UnEntier) is T) then
  Raise EContrainteArgumentException.Create('L''utilisation du type Integer n''est pas autoris� pour la Classe MaClasse<T>');
  //Writeln(Self.Classname); //E2003 Identificateur non d�clar� : 'Self'
end;

var Objet : MaClasse<Integer> ;
    Objet2 : MaClasse<String>;
    Objet21 : MaClasse<TObject>;
    Objet3,Objet4 : MaClasse<Double>;
    Objet5 : MaClasse<Byte>;

begin
try
 Writeln('D�but d''ex�cution du code.');
  //Premi�re r�f�rence d'un objet la classe dans le code, pas d'appel du constructeur de classe.
 Writeln(#13#10+'R�f�rence de la classe dans le code : Objet:=Nil');

 // Partage de code IL pour tous les types r�f�rences
  //Appel du constructeur de classe, le type n'existe pas encore
 Objet2:=nil;
 Objet2:=MaClasse<String>.Create;
  //Appel du constructeur de classe, le type n'existe pas encore
 Objet21:=nil;
 Objet21:=MaClasse<TObject>.Create;

 // Cr�ation de code pour chaque type valeur
  //Appel du constructeur de classe, le type n'existe pas encore
 Objet3:=nil;
 Objet3:=MaClasse<Double>.Create;

  //Pas d'appel du constructeur de classe, le type existe d�j�
 Objet4:=nil;
 Objet4:=MaClasse<Double>.Create;

  //Appel du constructeur de classe, le type n'existe pas encore
 Objet5:=nil;
 Objet5:=MaClasse<Byte>.Create;

  //Appel du constructeur de classe, le param�tre de type Integer est interdit.
 Objet:=nil;
 Objet:=MaClasse<Integer>.Create;

 except
    //D�clench�e par le constructeur de la classe g�n�rique ferm� MaClasse<Integer>
   on E:TypeInitializationException do
    If E.InnerException is EContrainteArgumentException
     then Writeln(E.InnerException.Classname, ': ', E.InnerException.Message)
     else Raise;

   on E:Exception do
     Writeln(E.Classname, ': ', E.Message);
 end;
 Readln;
end.


