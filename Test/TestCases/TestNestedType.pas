unit TestNestedType;

{ AFS
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
}

interface

implementation

type
Test1 = class
public
  type
    TTest2 = class
    end;

  var
    blabla: integer;

  const
    toto = '';

  class var
    tutu: string;

private

end;

end.


