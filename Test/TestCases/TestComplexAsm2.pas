unit TestComplexASM2;

{ AFS 9 October 2006 

 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility
}

interface

uses
  SysUtils;

type
  /// TMyClass comment
  TMyClass= class (TObject)
   procedure DoAsm; virtual;
  end;

implementation

uses DB;

/// DoAsm comment
procedure TMyClass.DoAsm;
asm
mov edx, [eax] // récupération de la VMT

// test du cas où le remplacement a déjà été fait
mov ecx, dword ptr TField.GetData
cmp dword ptr [edx + VMTOFFSET TField.IsBlob], ecx
je @@AlreadyDone

// sauvegarde du pointeur vers Draw dans OriginalDraw
mov ecx, dword ptr [edx + VMTOFFSET TField.IsBlob]
//mov dword ptr [edx + DMTOFFSET TField.GetData], ecx
// remplacement par DerivedDraw
mov ecx, dword ptr TField.IsBlob
mov dword ptr [edx], ecx

@@AlreadyDone :
end;

end.
