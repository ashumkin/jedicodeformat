program TestGenericArray;
{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type

 GenArray1Dim<T> = array of T;
 GenArray2Dim<T> = array of GenArray1Dim<T>;

 ArrInt1Dim = GenArray1Dim<Integer>;
 ArrInt2Dim = GenArray2Dim<Integer>;
 T2Dim= array of array of Integer;

{Main}
var
 TabInt : ArrInt1Dim;
 Tab2Int : ArrInt2Dim;
 TabType2Dim : T2Dim;

begin
 try
  TabInt:=New(GenArray1Dim<Integer>,2);
  TabInt:=New(ArrInt1Dim,2);

  Tab2Int:=New(T2Dim,2,2);
  Tab2Int:=New(ArrInt2Dim,2,2);

  TabType2Dim:=New(T2Dim,2,2);
  TabType2Dim:=New(ArrInt2Dim,2,2);

 except
   on E:Exception do
     Writeln(E.Classname, ': ', E.Message);
 end;
end.

