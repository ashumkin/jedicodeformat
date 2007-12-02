program TestGenericTypeNullable;

{
Test code from Trident for generics
}

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
 // System.Nullable est une structure
 // public struct Nullable<T> where T : struct
 TTestNullable<T:Record>=Class
   procedure Inverser(Arg:System.Nullable<T>; Valeur:System.Nullable<T>);
 End;

 MonRecord<T>=Record
   Data:T;
 End;

 MonRecordInt=Record
   Data:Integer;
   Constructor Create (Value:Integer);
   class operator Implicit(a: Integer): MonRecordInt;
 End;

 Couleurs=(Noir,Rouge,Vert);

Constructor MonRecordInt.Create(Value:Integer);
begin
 Data:=Value;
End;

class operator MonRecordInt.Implicit(a: Integer): MonRecordInt;
begin
 Result.Data:=a;
end;

//Avec Procedure TTestNullable<T>.Tester(Arg:System.Nullable<T>; Valeur:T); on ne peut assigner Null à Arg !
Procedure TTestNullable<T>.Inverser(Arg:System.Nullable<T>; Valeur:System.Nullable<T>);
//renseigne Arg avec Valeur si arg est null
//renseigne Arg avec null si arg est renseigné
begin
  try
   Writeln('Valeur du type nullable =',TObject(Arg.Value));
   Arg:=Default(Nullable<T>); //On bascule la valeur à Null
  Except
   On E:InvalidOperationException do  //Arg est null
    begin
     Writeln('La variable est à null');
     Arg:=Valeur; //On initialise la valeur (qui peut être null)
     Write('Nouvelle valeur :');
     if Valeur.HasValue          //Si la nouvelle valeur n'est pas null on l'affiche
      then Writeln(Tobject(Arg.Value))
      else Writeln('NULL');
    end;
 end;
end;

procedure Test(Arg:System.Nullable<integer>);
//Test, en utilisant la propriété HasValue, si la valeur d'un type nullable est Null ou pas
begin
  if Arg.HasValue
   then writeln('La variable n''est pas à null')
   else writeln('La variable est à null')
end;

procedure Test2(Arg:System.Nullable<integer>);
//Test, en utilisant l'accés à la propriété Value, si la valeur d'un type nullable est Null ou pas
begin
 try
  Writeln('Valeur du type nullable =',Arg.Value);
  Except
    //l'accés à la propriété Value déclenche une exception si son contenu est Null
   On E:InvalidOperationException do
    Writeln('La variable est à null')
 end;
end;

procedure Assignation;
var  I:Integer;
     intNull:System.Nullable<integer>;

begin
  intnull:=52;
  //I:=intNull;  //E2010 Types incompatibles : 'Integer' et 'Nullable<System.Int32>'
  I:=Integer(intNull);  //Transtypage obligatoire
  Writeln('I=',I);
  Writeln('intNull=',intNull,'intNull.TString=',intNull.ToString);
  I:=Convert.ToInt32(intNull);
  Writeln(I);
Writeln('Gestion d''une valeur null');
  //intnull:=Nil; //E2010 Types incompatibles : 'Nullable<System.Int32>' et 'Pointer'
  intnull:=Default(Nullable<integer>);
  // Si intNull est à null l'instruction suivante déclenche l'exception InvalidOperationException
  //I:=Integer(intnull);
  I:=intNull.GetValueOrDefault;
  Writeln('intNull=',intNull,'intNull.TString=',intNull.ToString);
  I:=Convert.ToInt32(intNull);
  Writeln(I); //i=0

  try
   I:=intNull.Value; //Compile
  Except
    //l'accés à la propriété Value déclenche une exception si son contenu est Null
   On E:InvalidOperationException do
    Writeln('L''assignation ne peut s''effectuer la variable intNull est à null');
 end;
 I:=intNull.GetValueOrDefault; //évite le try except si l'état null n'est pas déterminant
end;

procedure Addition;
var  I       : Integer;
     J       : System.Nullable<integer>;
     intNull : System.Nullable<integer>;

begin
  IntNull:=1;
  //intNull:=intNull+1; //E2015 Opérateur non applicable à ce type d'opérande
  //intNull:=intNull+System.Nullable<integer>(1); //Idem
  //inc(intNull); //Idem
  //inc(intNull.value); //E2064 La partie gauche n'est pas affectable

  IntNull:=Integer(intNull)+1;
  intNull:=intNull.value+1;
  with intNull do
   intNull:=value+1;

  //I:=intNull+5; //E2015 Opérateur non applicable à ce type d'opérande
  I:=Integer(intNull)+5;
  I:=intNull.value+5;
  I:=7;
  J:=12;
  intnull:=J;
  intnull:=8;

 //Addition de deux variable de type null
  //intnull:=intNull+J; //E2015 Opérateur non applicable à ce type d'opérande
  //intnull:=intNull.Value+J; //idem
  intnull:=intNull.Value+J.Value;
  //with intNull do
   // Value:=Value+J.Value;   //E2129 Affectation impossible à une propriété en lecture seule
end;

procedure Addition2;
//Addition entre un type integer nullable et un type double nullable
var  J:Double;
     intNull : System.Nullable<integer>;
     dblNull : System.Nullable<Double>;

begin
  intNull:=1;
  dblNull:=1.0;

  intNull:=intNull.value+1;   //Call opérateur Implicit
  intNull:=Integer(intNull)+1; //Call opérateur Explicit

  J:=intNull.value+1.5;
  Writeln('J+integer=',J);
  dblNull:=Integer(intNull)+1.5;
  Writeln('Double+integer=',dblNull);
  //Intnull:=Integer(dblNull); //E2089 Transtypage incorrect
  //Intnull:=Integer(dblNull.Value); //Idem
  //Intnull:=Round(dblNull); // E2015 Opérateur non applicable à ce type d'opérande
  Intnull:=Round(dblNull.Value);
end;


Var NullInteger : System.Nullable<integer>;
    TypeNull:TTestNullable<Integer>;
    //TpNull:TTestNullable<String>; //E2512 Le paramètre type 'T' doit être un type de valeur non null
    //TpNull:TTestNullable<MonRecord<Integer>>;  //E2541 Un tableau statique, une chaîne courte ou un type enregistrement contenant un champ avec ces types ne peut pas être utilisé comme argument de type
    TypeNull2:TTestNullable<MonRecordInt>;
    RecNullable :System.Nullable<MonRecordInt>;
    Rec:MonRecordInt;
    EnumNullable :System.Nullable<Couleurs>;
    UneCouleur: Couleurs;
begin
  try
  Assignation;
  Addition2;
  Addition;
  Writeln('Test 1');
   Test(NullInteger);
   NullInteger:=256;
   Test(NullInteger);
   NullInteger:=Default(Nullable<integer>);
   Test2(NullInteger);
   Writeln;

  Writeln('Test 2');
   TypeNull:=TTestNullable<Integer>.Create;
   With TypeNull do
   begin
    Inverser(NullInteger,256);
    Inverser(NullInteger,Default(Nullable<integer>));
   end;
   Writeln;

  Writeln('Test 3');
   TypeNull2:=TTestNullable<MonRecordInt>.Create;
   {TypeNull2.Inverser(RecNullable,852); //E2010 Types incompatibles : 'Nullable<MonRecordInt>' et 'Integer'
   }
    RecNullable:=MonRecordInt.Create(852);
    Writeln(RecNullable.Value.Data);
    //RecNullable:=10; //E2010 Types incompatibles : 'Nullable<MonRecordInt>' et 'Integer'
    //RecNullable.Value:=10; //E2129 Affectation impossible à une propriété en lecture seule
    //RecNullable.Value.Data:=10; //E2064 La partie gauche n'est pas affectable
    Rec:=RecNullable.Value;
    Rec.Data:=10;
    RecNullable:=Rec;
    RecNullable:=MonRecordInt(99);  //Appel implicit
    Writeln(RecNullable.Value.Data);
    TypeNull2.Inverser(RecNullable,Rec);
    NullInteger:=Default(Nullable<Integer>);
   Test(NullInteger);

  Writeln('Test 3');
    EnumNullable:=Rouge;
    case EnumNullable.Value of
     Noir : Writeln('Couleur Noir');
     Rouge : Writeln('Couleur Rouge');
     Vert : Writeln('Couleur Vert');
    end;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
   Readln;
end.
