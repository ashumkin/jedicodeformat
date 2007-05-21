unit TestDelphiNetRecordClassVars;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
 Testing records with class vars and class operators
 code is after a e.g. from Caleb Hattingh
}

interface

uses
  SysUtils,
  Math;

type

  TAFish = record
    class var aString: string;
  private
    class var scratch: double;
  public
    class operator Add(AValue: TAFish) : TAFish;
    class var fooFactor: double;
  end;

  AFishClass = class
    class var aString: string;
  private
    class var scratch: double;
  public
    class operator Add(AValue: TAFish) : TAFish;
    class var fooFactor: double;
  end;


implementation

{ TAFish }

class operator TAFish.Add(AValue: TAFish): TAFish;
begin
  Result := AValue;
end;

{ AFishClass }

class operator AFishClass.Add(AValue: TAFish): TAFish;
begin
  Result := aValue;
end;

end.