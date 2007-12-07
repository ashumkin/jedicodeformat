program TestGenericClassHelper;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type
  TGenerique<T>  = class
   Champ: T
  end;

  {
   E2003 Identificateur non d�clar� : 'T'
  THelperGenerique=Class Helper for TGenerique<T>
   Procedure Test;
  End;

   E2508 Les param�tres de type ne sont pas autoris�s sur ce type
  THelperGenerique<T>=Class Helper for TGenerique<T>
   Procedure Test;
  End;
 }
  TGeneriqueInt=TGenerique<Integer>;
  THelperGeneriqueInt=Class Helper for TGeneriqueInt
   Procedure Test;
  End;

  TGeneriqueString=TGenerique<String>;

Procedure THelperGeneriqueInt.Test;
begin
  Writeln('Proc�dure fournie par un assistant de classe.');
end;


var Assistant :TGeneriqueInt;
    Assistant2 :TGeneriqueString;
begin
  try
   Assistant:=TGeneriqueInt.Create;
   Assistant.Test;
   //Assistant2:=TGeneriqueString.Create;
   //Assistant2.Test;   //E2003 Identificateur non d�clar� : 'Test'
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  Readln;
end.
