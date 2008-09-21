unit TestClassOf;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
}

interface

uses classes;

type
  MyStringListClass = class of TStringList;
  MyStringNamespacedListClass = class of Classes.TStringList;

implementation

end.
