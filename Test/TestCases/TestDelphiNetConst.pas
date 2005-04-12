unit TestDelphiNetConst;

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

implementation

{ TMyClass }

procedure TMyClass.Hello;
begin

end;

end.
