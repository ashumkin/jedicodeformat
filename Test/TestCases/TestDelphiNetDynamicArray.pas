unit TestDelphiNetDynamicArray;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Tests of the Delphi.net 'new' keyword for array creation
 from TridenT
}

interface

implementation

uses
  SysUtils;

type 
 TObjectDynArray = array of TObject; 

Procedure TabObjet(Args : array of TObject); 
var i: Integer; 
begin 
 Writeln('Dans la procédure TabObjet'); 
 for i := Low(Args) to High(Args) do 
  WriteLn('L''élément ', I, ' est du type ', Args[i].GetType.FullName,' sa valeur est ''',Args[i],''''); 
 ReadLn; 
end; 

procedure TabConst(A: array of const); 
var i: Integer; 
begin 
 Writeln('Dans la procédure TabConst'); 
 for i := Low(A) to High(A) do 
 // WriteLn('Index ', I, ': ', A[i].GetType.FullName); 
  WriteLn('L''élément ', I, ' est du type ', A[i].GetType.FullName,' sa valeur est ''',A[i],''''); 
 ReadLn; 
end; 

procedure Test02;
var
  X,Y: array[,] of Integer; // 2 dimensional array
  i,j: Integer;
begin
  i := 2;
  j := 3;
  X := New(array[i,j] of Integer); // only size of 2x3 matrix
  X[0,0] := 1;
  X[0,1] := 2;
  X[0,2] := 3;
  X[1,0] := 4;
  X[1,1] := 5;
  X[1,2] := 6;
  // type and initializer list
  Y := New(array[,] of Integer, ((1,2,3), (4,5,6)));
  for i:=0 to 1 do
    for j:=0 to 2 do
      if X[i,j] <> Y[i,j] then writeln(i,j);
  writeln('done');
  readln;
end;


  // Déclare et initialise qq variables 
Var S1    : String='Une chaîne'; 
    I     : Integer=90; 
    D1    : Double=5.6; 
    D2    : Double=3.14159; 
    Etat  : Boolean=True; 
    S2    : String='s'; 

    TbObj : Array of TObject; 

begin 
  // Passage d'un nombre de paramètre variable sans préciser le type, uniquement des constantes 
 TabConst(['Une chaîne', 90, 5.6, 3.14159, True, 's']); 
  // Passage d'un nombre paramètre variable sans préciser le type, mixte constantes et variables 
 TabConst([S1, I, 5.6, D2, Etat, 's']); 
  // Dans le second appel le type du 2 éme paramètre est différent 
  // A la compilation 90 est vu comme un type Byte et I est de type Integer 

  // Création dynamique d'un tableau 
 TbObj:=New(TObjectDynArray,6); 
  // La directive AutoBox permet d'éviter le transtypage explicite nécessaire pour chaque élément du tableau 
  // Cette directive effectue implicitement le cast 
 {$AutoBox on} 
     // Renseigne le tableau avec des variables 
   TbObj:= New(array[] of TObject, (S1, I, D1, D2, Etat, S2)); 
 {$AutoBox off} 
 TabObjet(TbObj); 

 {$AutoBox on} 
    // Renseigne le tableau avec des variables et des constantes 
   TbObj:= New(array[] of TObject, (S1, I, 5.6, D2, Etat, 's')); 
 {$AutoBox off} 
 TabObjet(TbObj); 
end.
