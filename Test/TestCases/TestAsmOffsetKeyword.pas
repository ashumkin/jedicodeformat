unit TestAsmOffsetKeyword;


{ AFS Apil 2008
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
}

interface

uses
  SysUtils;

type
  TMyClass= class (TObject)
   Field1 : integer ;
   procedure DoSomeThing(Offset, x, y, dx, dy : integer); virtual;
  end;

implementation

/// FaitqqChose comment
procedure TMyClass.DoSomeThing(Offset, x, y, dx, dy: integer);
asm
PUSH dy;
PUSH &dx;
PUSH y;
PUSH x;
TEST &Offset, &Offset;
end;

end.

