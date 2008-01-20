unit LittleTest26;

{ AFS August 2003
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
 Test the "packed" keyword }


interface

type
  TFooRecord = packed record
    Soy: integer;
    Monkey: integer;
    Shatner: integer;
    McFlurry: integer;
  end;

  type aPackedArray = Packed array[0..4] of integer;

  type aPackedClass = Packed class
    Soy: integer;
    Monkey: integer;
  end;

const
  MyFooRecord: TFooRecord = (
    Soy: 0;
    Monkey: 0;
    Shatner: 0;
  );

implementation

end.
