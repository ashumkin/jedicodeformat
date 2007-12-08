program TestGenericMethods1;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;
type
   // méthode générique sur une classe générique
  TGenericType<AnyType> = class
    FData: AnyType;
    function GetData: AnyType;

     //ici l'usage d'Anytype génère le warning :
     // H2509 Identificateur 'AnyType' en conflit avec les paramètres type du type conteneur
    procedure UneMethodeGenerique<AnyType>(Variable: AnyType);
    function UneFonctionGenerique<AnyType>: AnyType;

    procedure SetData(Value: AnyType);
    property Data: AnyType read GetData write SetData;
  end;

function TGenericType<AnyType>.GetData: AnyType;
begin
  Result := FData;
end;

procedure TGenericType<AnyType>.SetData(Value: AnyType);
begin
  FData := Value;
end;

procedure TGenericType<AnyType>.UneMethodeGenerique<AnyType>(Variable: AnyType);

begin
  Variable:=Default(AnyType);
  Writeln(TObject(Variable).ClassName);
  //Writeln('Default=',Variable); //E2054 cast obligatoire pour Writeln
  Writeln('Default pour AnyType=',TObject(Variable).ClassName,'=', TObject(Variable));
  Writeln(TObject(Variable).ClassName);
end;

function TGenericType<AnyType>.UneFonctionGenerique<AnyType>: AnyType;
var Variable : AnyType;
begin
  //Result := AnyType;   //'(' attendu mais ';' trouvé (E2029). On manipule un type par une variable
  Variable:=Default(AnyType);
  Writeln(TObject(Variable).ClassName);
  Writeln('Default pour AnyType=',TObject(Variable).ClassName,'=',TObject(Variable));
  Result := Variable;
end;


type
  TGenericTypeInt = TGenericType<Integer>;
  //TGenericTypeString = TGenericType<string>;
  TGenericTypeBoolean = TGenericType<Boolean>;

var
  I: TGenericTypeInt;
  //S: TGenericTypeString;
  B:TGenericTypeBoolean;
begin
 try
  I := TGenericTypeInt.Create;
  I.Data := 100;
  I.UneMethodeGenerique(I.Data);

  {S := TGenericTypeString.Create;
  S.Data := 'Chaîne de cractères';
  S.UneMethodeGenerique(S.Data);
  S.UneMethodeGenerique<Integer>(I.Data);
  }
  B := TGenericTypeBoolean.Create;
  B.Data := True;
  B.UneMethodeGenerique(B.Data);
  B.UneMethodeGenerique<Integer>(I.Data);
  ReadLn;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

