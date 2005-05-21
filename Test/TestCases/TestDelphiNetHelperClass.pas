unit TestDelphiNetHelperClass;

{
  AFS May 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Basic test of helper class in Delphi.NET
}


interface

type
TMyClass = class
  public
    procedure HelloMyClass;
  end;

  TMyClassHelper = class helper for TMyClass
  public
    procedure HelloMyClassHelper;virtual;
  end;
 
  TMyClassHelper2 = class helper(TMyClassHelper) for TMyClass
  public
    procedure HelloMyClassHelper;override;
  end;

implementation

{ TMyClass }

procedure TMyClass.HelloMyClass;
begin

end;

{ TMyClassHelper }

procedure TMyClassHelper.HelloMyClassHelper;
begin

end;

{ TMyClassHelper2 }

procedure TMyClassHelper2.HelloMyClassHelper;
begin
  inherited;

end;

end.
