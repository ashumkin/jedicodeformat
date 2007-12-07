program TestGenericConstraintConstructor;

{$APPTYPE CONSOLE}

{ AFS December 2007 

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test new generics syntax - code from TridenT
}

uses
  SysUtils;

type
  TGenericSansConstructeur<T> = class
  strict private
    FData: T;
    constructor Create; // inaccessible pour la contrainte :Constructor
  public
    function GetData: T;
    procedure SetData(Value: T);
    property Data: T read GetData write SetData;
  end;

  TGenericAvecConstructeur<T> = class
  private
    FData: T;
  public
    constructor Create;
    function GetData: T;
    procedure SetData(Value: T);
    property Data: T read GetData write SetData;
  end;


 TGenericContraint<ClasseInstanciable:Constructor>=Class
  private
   MaClasse : ClasseInstanciable;
  public
   Constructor Create;
 end;

 MonTypeGeneric  = TGenericAvecConstructeur<Integer>;

constructor TGenericSansConstructeur<T>.Create;
begin
 Inherited Create;
end;

function TGenericSansConstructeur<T>.GetData: T;
begin
  Result := FData;
end;

procedure TGenericSansConstructeur<T>.SetData(Value: T);
begin
  FData := Value;
end;

{ TGenericAvecConstructeur<T> }

constructor TGenericAvecConstructeur<T>.Create;
begin
 Inherited Create;
end;

function TGenericAvecConstructeur<T>.GetData: T;
begin
 Result := FData;
end;

procedure TGenericAvecConstructeur<T>.SetData(Value: T);
begin
 FData := Value;
end;

var
  X  : TGenericSansConstructeur<String>;
  Y  : MonTypeGeneric;
  Z  : TGenericContraint<TGenericSansConstructeur<Integer>>;
  Z2 : TGenericContraint<TGenericAvecConstructeur<Integer>>;
  Z3 : TGenericContraint<TGenericAvecConstructeur<TObject>>;
  Z4 : TGenericContraint<MonTypeGeneric>;

{ TGenericDerivee<ClasseInstanciable> }

constructor TGenericContraint<ClasseInstanciable>.Create;
begin
 Inherited Create;
  //Sans la contrainte Constructor on obtient l'erreur :
  // E2076 : Forme d'appel de méthode autorisée seulement pour méthodes de classe
 MaClasse:=ClasseInstanciable.Create;
end;

begin
 try
  //Sans la contrainte Constructor le code suivant compile.
  //Avec la contrainte Constructor on obtient l'erreur :
   //E2513 : Le paramètre type 'ClassInstanciable' doit avoir un constructeur sans paramètre
  //Z := TGenericContraint<TGenericSansConstructeur<Integer>>.Create;
    //Type incompatible
  //Z := TGenericContraint<TGenericAvecConstructeur<Integer>>.Create;
  Z2:=TGenericContraint<TGenericAvecConstructeur<Integer>>.Create;
  WriteLn(Z2.MaClasse.ToString);
  //Z2.Free; // Sous .NET pas nécessaire. En Win32 la libération des ressources devra en tenir compte.
  Z3:=TGenericContraint<TGenericAvecConstructeur<TObject>>.Create;
  WriteLn(Z3.MaClasse.ToString);

  Z4:=TGenericContraint<MonTypeGeneric>.Create;
   // Ne compile pas.
  //Z4:=TGenericContraint<Y>.Create;
  WriteLn(Z4.MaClasse.ToString);
 except
   on E:Exception do
     Writeln(E.Classname, ': ', E.Message);
 end;
  ReadLn; 
end.
