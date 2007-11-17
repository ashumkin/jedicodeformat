unit TestAsmCaps;

{ AFS November 20077

  This code compiles, but is not semantically meaningfull.
  It is test cases for the code-formating utility

  Test capitalisation in asm
}

interface

implementation


var
  _NetGroupDel: Pointer;

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
begin
end;

procedure NetGroupDel;
begin  
  GetProcedureAddress(_NetGroupDel, 'Foo', 'bar');
  asm
           mov     ESP, EBP
           pop     EBP
           jmp     [_NetGroupDel]
  end;
end;

end.


