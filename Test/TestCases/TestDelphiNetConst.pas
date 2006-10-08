unit TestDelphiNetConst;

{
  AFS May 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Basic test of class constants in Delphi.NET
}

interface

type
  TTemperatureConverter = class(TObject)
  public
    const AbsoluteZero = -273;
  end;

type
  TMyClass = class
    const
      x = 12;
      y = TMyClass.x + 23;
    procedure Hello;
  private
    const
      s = 'A string constant';
  end;

  TTestConstFirst = class(TObject)
    const SOME_CONST = 1;
    private
      testInt: Int32;
   end;

  TTestConstFirst2 = class(TObject)
    const 
      SOME_CONST = 1;
      OTHER_CONST = 2;
    private
      testInt: Int32;
  end;


implementation

{ TMyClass }

procedure TMyClass.Hello;
begin

end;

end.
