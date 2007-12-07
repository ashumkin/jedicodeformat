program TestGenericDelegates;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;
type
  TMammifere=Class
   Procedure GetNom;Virtual;
  End;

  TChien=Class(TMammifere)
     Procedure GetNom;override;
  End;

   // Definie les délégués.
  THandlerMethod<T:TMammifere>=Function:T of object;

  TMonDelegue = THandlerMethod<TMammifere>;
  TMonDelegue2= THandlerMethod<TChien>;

  TClasse1=Class
   function FirstHandler:TMammifere;
   function SecondHandler:TChien;
  end;

{ TClasse1 }
function TClasse1.FirstHandler: TMammifere;
begin
 result:=TMammifere.Create;
end;

function TClasse1.SecondHandler: TChien;
begin
 result:=TChien.Create;
end;

{ TMammifere }
procedure TMammifere.GetNom;
begin
 Writeln('TMammifere');
end;

{ TChien }
procedure TChien.GetNom;
begin
  inherited;
 Writeln('TChien');
end;

var Classe:TClasse1;

    handler1 : TMonDelegue;
    handler2 : TMonDelegue2;
    Hdl : System.Delegate;
    resultat : TMammifere;

begin
 Classe:=Nil;
 try
  Classe:=TClasse1.Create;
  handler1:=@Classe.FirstHandler;
   // A la différence du C# la covariance n'est pas permise.
   //avec handler2 : TMonDelegue;
  //handler2:=@Classe.SecondHandler;

  Resultat:=handler1;
  Resultat.GetNom;

  handler1:=@Classe.SecondHandler as TMethod;
  Resultat:=handler1;
  Resultat.GetNom;

  Hdl:=@Classe.FirstHandler;
  Resultat:=(hdl as TMonDelegue);
  Resultat.GetNom;

  Hdl:=@Classe.SecondHandler;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.

