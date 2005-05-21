unit TestDelphiNetStatic;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 basic tests of class procedures and class constructors
}

interface

type
  TMyClass = class(TObject)
  class var
    InstanceCount:Integer;
  end;

type
  TMyClass2 = class(TObject)
    public
      class procedure ShowInstanceCount; static;
  end;

type
  TMyClass3 = class(TObject)
  private
    class procedure SetData(AData:Integer);static;
    class function GetData:Integer;static;
    class var
      FData : Integer;
  public
    class property Data:Integer read GetData write SetData;
  end;

type
  TMyClass4 = class(TObject)
  public
    class constructor Create;
  end;

type
  TMyClass5 = class sealed
  public
    procedure HelloMyClass;
  end;

implementation

{ TMyClass2 }

class procedure TMyClass2.ShowInstanceCount;
begin

end;

{ TMyClass3 }

class procedure TMyClass3.SetData(AData: Integer);
begin
  fData := aData;
end;

class function TMyClass3.GetData: Integer;
begin
  Result := fData;
end;

{ TMyClass4 }

class constructor TMyClass4.Create;
begin

end;

{ TMyClass5 }

procedure TMyClass5.HelloMyClass;
begin

end;

end.
