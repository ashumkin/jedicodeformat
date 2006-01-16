unit TestAsmLabel;

{ AFS 15 Jan 2006
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

  Test labels and numeric consts in asm
 }


interface

implementation


Procedure Test;
Label
  lbl;
Asm
  lbl: mov eax, [eax+20h]
End;

end.