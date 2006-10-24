unit LittleTest27;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
 to test the "deprecated" keyword }

interface

uses SysUtils;

const
  Captain = 'Kirk' deprecated; // Try new improved Picard, no toupee required!
  Wizard = 'Gandalf' platform; // to stand on, makes him look taller.
  Librarian = 'Monkey' library; // ook ook!

  // typed constants too 
  Joke: String = 'A frayed knot' deprecated;

  OverAchiver: integer = 3 deprecated platform library;

type
  TSetOfChars = TSysCharSet deprecated;

  TMyClass = class
  protected
    AVariable : integer deprecated;
end;



implementation

end.
