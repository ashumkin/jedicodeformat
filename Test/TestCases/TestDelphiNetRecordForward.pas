unit TestDelphiNetRecordForward;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Delphi.NEt record forward declarations
}

interface

type

  TRecord1 = record;
  TRecord2 = record;
  TRecord3 = record;

  TRecord1 = record
    foo: integer;
    bar: string;
    fish: double;

    rec: TRecord2;
  end;

  TRecord2 = record
    foo: integer;
    bar: string;
    fish: double;

    rec: TRecord3;
  end;

  TRecord3 = record
  end;


implementation

end.

