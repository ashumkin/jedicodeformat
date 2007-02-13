unit TestAsmOps;

{ AFS 27 March 2000
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

  Sourceforge bug #1567643
}

interface

uses windows;

implementation

Function ASMSQRT(F: Single): Single; Assembler;
asm
  FLD F;
  FSQRT;
  FSTP Result;
end;

procedure foo;
asm
  //blabla1
  movsd; movsd; movsd // blabla2
  movsd //blabla3
end;

end.
