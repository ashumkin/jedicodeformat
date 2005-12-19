unit TestDelphiNetSealedClass;

{ AFS 19 Dec 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Test Delphi.net syntax for sealed and abstract classes
   Code by TridenT
}

interface

uses
  SysUtils;

type
  TMaClass= class (TObject)
   Champ1 : integer ;
   procedure FaitqqChose; virtual;
  end;

  TSealedClass= class sealed (TMaClass)
   Champ2 : integer ;
   procedure FaitqqChose; override;
  end;


  TAbstractClass= class abstract (TObject)
   Champ2 : integer ;
   procedure FaitqqChose; virtual; abstract;
  end;

implementation

procedure TmaClass.FaitqqChose;
begin
 Writeln('Champ1 ', Champ1);
end;

procedure TSealedClass.FaitqqChose;
begin
 Writeln('Champ2 ', Champ2);
end;

var
 MonInstance: TSealedClass;

begin
  MonInstance:=TSealedClass.Create;
end.
