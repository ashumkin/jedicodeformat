program TestGenericConstructeurStatic;
{$APPTYPE CONSOLE}

(* Adaptation du code C# suivant : http://www.dotnetguru.org/articles/dossiers/generics/CS2_Generics_FR2.htm#_Toc87323770
class C<T>{
   static C(){
      int a=0;
      if( ((object) default(T) != null) && a is T)
         throw new ArgumentException("Not allowed to use the type C<int>.");
   }
}
*)

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
{        ATTENTION
Le CLR de .NET cr�e une copie sp�cialis�e du code native pour chaque instanciation
de type g�n�rique de type valeur, mais partage une simple copie du code natif pour
tous les types r�f�rences (puisque, au niveau de code natif, les r�f�rences ne sont
que des pointeurs avec la m�me repr�sentation).
}
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


