unit TestBogusDirectives;

{ AFS 11 Jan 2000
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility


 This unit has code that compiles, but is positively pathological,
 it mimics variable and proc names that are textualy identical to directives
 This is legal, but not advised
}

interface

uses Classes;

{ this also serves as a test of var layout }
var Safecall,
DispId: integer;
Cdecl, Protected: boolean;

const
ReadOnly = 10;

type
  Register = class (TObject)
    private
      fiDynamic, fiPublic: integer;
    function GetReadOnly: Boolean;
    protected
    public
      property Dynamic: integer rEad fiDynamic wRite fiDynamic;
      property Public: integer read fiPublic;
      property ReadOnly: Boolean rEAd GetReadOnly;

      function DispId: integer; SAFECAlL;
      function ProtecTED: boolean;

      { are these keywords or directives? }
      procedure Contains;
      procedure Requires;
      procedure Package;
      function At: integer;
      function On: integer;
      procedure Final(buffer: TStrings);
  end;

implementation

label dynamic;

var
  platform: integer;


procedure Override;
var
  Virtual: integer;
begin
  Virtual := 10;
end;

{ register }

function Register.Dispid: integer;
begin
  Result := 12;
end;

function Register.GetReadOnly: Boolean;
var
  Name: String;
  Absolute: integer    absolute            Name;
begin
  Name := 'Fred!';
  Result := True;
end;

function Register.ProtecTED: boolean;
begin
  Result := False;
end;

procedure Register.Contains;
var
  platform: array [1..4] of char;
begin
end;

procedure Register.Requires;
begin
end;

procedure Register.Package;
begin
end;

function Register.At: integer;
begin
  Result := 23;
end;

function Register.On: integer;
begin
  Result := 23;
end;

procedure Register.Final(buffer: TStrings);
begin
end;

procedure FinalAbuse(buffer: TStrings);
begin
  with Register.Create do
  try
    Final(buffer);
  finally
    Free;
  end;
end;


end.
