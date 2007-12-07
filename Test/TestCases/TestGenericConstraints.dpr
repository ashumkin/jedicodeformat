program TestGenericConstraints;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils,Classes;
type
 Couleurs=(Rouge,Noir,Vert);

 IMonInterface=interface
  procedure FaitqqChose;
 end;

 TClasseMonInterface=Class(TObject,IMonInterface)
   procedure FaitqqChose;
 End;

 TEnregistrement=Record //Record non g�n�rique (peut impl�menter des interfaces)
 End;

 TGenericEnregistrement<T>=Record //Record g�n�rique, attend un param�tre de type non contraint (de tout type)
  Data: T;
 End;


 TGenericClass<R:Record>=Class //Contrainte record sur une classe, attend un param�tre de type d'un type valeur
  Champ: R;
 End;


 TGenericRecord<C:Class>=Record //Contrainte class sur un enregistrement, attend un param�tre de type du type Classe
  MaClasse : C;
 End;

 TGenericClass2<C:Class>=Class //Contrainte class sur une classe , attend un param�tre de type du type Classe
  Valeur : C;
 End;


 TGenericRecord2<C:TList>=Record //Contrainte sur une classe anc�tre, attend un param�tre de type du type TList et d�vir�e
  MaClasse : C;
 End;
 
 TGenericRecord3<C:Class,Constructor>=Record //Les contraintes peuvent �tre coupl�es.
  MaClasse : C;
 End;


 //TGenericClass2<U:TEnregistrement>=Class; //E2510 Le type TEnregistrementn'est pas une contrainte valide
 //Pas d'h�ritage pour les record sous .NET. Un record est un type valeur

 //TGenericClass2<U:TGenericClass<R:Record>>=Class // E2003: Identificateur non d�clar� : 'R'
 // N�cessite un type ferm�.
 TGenericClass3<U:TGenericClass<TEnregistrement>>=Class //Contrainte sur une classe g�n�rique particuli�re
  EnregistrementUn: U;
 End;

 TGenericClass4<U:TGenericClass<TEnregistrement>>=Class
  EnregistrementUn: U;
  EnregistrementDeux: TGenericRecord<TObject>;
  EnregistrementTrois: TGenericRecord<TGenericClass<TEnregistrement>>;
  //E2541 Un tableau statique, une cha�ne courte ou un type enregistrement contenant un champ avec ces types ne peut pas �tre utilis� comme argument de type
  //EnregistrementQuatre: TGenericClass<TGenericRecord<TObject>>;
 End;

 TClassContrainteTypeNu<T,U>=Class
   type
     TClassImbriquee<S:U>=Class
      Valeur:S;
     End;
 End;


procedure TClasseMonInterface.FaitqqChose;
begin
  Writeln('Dans la proc�dure d''interface FaitqqChose');
end;

Var
 Rec :TGenericRecord<TObject>;
 ObjIntf : TGenericClass2<IMonInterface>; //La contrainte class accepte un type interface !
 Obj2 : TGenericClass2<TObject>;
 Obj3 : TGenericClass2<String>; //La contrainte class accepte le type String
 Obj4 : TGenericClass2<Array of Integer>; //La contrainte class accepte aussi un type tableau...

 Obj5: TGenericClass<TEnregistrement>;
 Obj6: TGenericClass<Integer>;
 Obj7: TGenericClass<Couleurs>;

 Parent:TClassContrainteTypeNu<String,Integer>;
 Naked : TObject;

begin
  try
   Rec.MaClasse:=TObject.Create;
    //Test
   ObjIntf:=TGenericClass2<IMonInterface>.Create;
   ObjIntf.Valeur:=TClasseMonInterface.Create;
   IMonInterface(ObjIntf.Valeur).FaitqqChose;

   Obj2:=TGenericClass2<TObject>.Create;
   Obj2.Valeur:=nil;
   Obj2.Valeur:=TObject.Create;

   Obj3:=TGenericClass2<String>.Create;
   Obj3.Valeur:='Toto';

   Parent:=TClassContrainteTypeNu<String,Integer>.Create;
   //E2515 Le param�tre type 'S' n'est pas compatible avec le type 'Integer'
   Naked:=TClassContrainteTypeNu<String,Integer>.TClassImbriquee<String>.Create;
 except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.
