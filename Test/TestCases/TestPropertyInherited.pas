Unit TestPropertyInherited;

{ bug #762354 in jcf 2 Beta 5
  reported by adem baba
}

Interface

const
  HAS_BAR = False;
  FIVE = (2 + 2);

type

TCustomFoo = class(TObject)
  private
    function GetFoo: integer;

  protected
    property Foo: integer read GetFoo nodefault;
    property Bar: integer read GetFoo stored HAS_BAR;
    Property Monkey: integer read GetFoo;
    Property Soy: integer read GetFoo;

end;

TFoo = class(TCustomFoo)
 public
  Property Foo;
End;

const
  FOO_DEFAULT = 3;

type
  TMegaFoo = class(TFoo)
  private
    function GetFishes(const piC: integer): integer;
    procedure SetFishes(const piC, Value: integer);
  public
    property Fishes[const piC: integer]: integer read GetFishes write SetFishes; default;

  published
    property Bar default 3;
    property Foo default FOO_DEFAULT + 1;
    property Monkey Stored FIVE;
    property Soy Stored FIVE + 1 nodefault;
  end;

  { base class with array and scalar property }
  TUserHasDefaults = class(TObject)
  private
    function GetDef1: integer;
    function GetDef2(const piC: integer): integer;
  public
    property Def1: integer read GetDef1;
    property Def2[const piC: integer]: integer read GetDef2;

  end;

  THasDefaults = class(TUserHasDefaults)
  // two different syntaxes - semicolon in one not other
    property Def1 default 1;
    property Def2; default;

  end;


Implementation

{ TCustomFoo }

function TCustomFoo.GetFoo: integer;
begin
  Result := 3;
end;

{ TMegaFoo }

function TMegaFoo.GetFishes(const piC: integer): integer;
begin
  Result := piC - 1; // overfishing
end;

procedure TMegaFoo.SetFishes(const piC, Value: integer);
begin

end;

{ TUserHasDefaults }

function TUserHasDefaults.GetDef1: integer;
begin
  Result := 0;
end;

function TUserHasDefaults.GetDef2(const piC: integer): integer;
begin
  Result := piC + 1;
end;

End.