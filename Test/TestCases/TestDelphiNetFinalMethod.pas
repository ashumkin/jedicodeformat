unit TestDelphiNetFinalMethod;

{ AFS 19 Dec 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

  Test Delphi.net syntax for final methods
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

  TDescendantClass= class (TMaClass)
   Champ2 : integer ;
   procedure FaitqqChose; override; Final;
  end;

implementation

procedure TMaClass.FaitqqChose;
begin
 Writeln('Champ1 ',Champ1);
end;

procedure TDescendantClass.FaitqqChose;
begin
 Writeln('Champ2 ',Champ2);
end;

end.
