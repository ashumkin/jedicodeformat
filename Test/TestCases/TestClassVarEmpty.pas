unit TestClassVarEmpty;

{ AFS May 2008
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 This unit tests empty class var sections
 as per sourceforge bug 1948057: Parser exception with empty class var
 reported by TridenT 
 }


interface

type
  TMyClass1 = class(TObject)
  private
    class var
  protected
    procedure SomeProtectedMethod;
end;

type
  TMyClass2 = class(TObject)
  private
    procedure SomeMethod;
  protected
    class var
  public
    function SomeFunction: boolean;
end;

type
  TMyClass3 = class(TObject)
  private
    procedure SomeMethod;
  protected
    class var
end;

type
  TMyClass4 = class(TObject)
  private
    procedure SomeMethod;
  protected
    class var
  public
end;

type
  TMyClass5 = class(TObject)
  private
    FFoo: integer;
    procedure SomeMethod;
  protected
    class var
  public
    property Foo: integer read FFoo write FFoo;
end;

  //test empty var
  TMyClassWithVar1 = class
  public
  const
    CONST_VALUE = 'value';
  var
  private
    FField : integer
  end;

  TMyClassWithVar2 = class
  var
  public
    FField : integer
  end;


implementation

{ TMyClass }

procedure TMyClass1.SomeProtectedMethod;
begin

end;

{ TMyClass2 }

function TMyClass2.SomeFunction: boolean;
begin
  Result := true;
end;


procedure TMyClass2.SomeMethod;
begin

end;

{ TMyClass3 }

procedure TMyClass3.SomeMethod;
begin

end;

{ TMyClass4 }

procedure TMyClass4.SomeMethod;
begin

end;

{ TMyClass5 }

procedure TMyClass5.SomeMethod;
begin

end;

end.
