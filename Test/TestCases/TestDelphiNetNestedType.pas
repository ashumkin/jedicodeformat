unit TestDelphiNetNestedType;

interface

type
  TOuterClass = class
  private
    myField: Integer;
  public
    type
      TInnerClass = class
      public
        myInnerField: Integer;
        procedure innerProc;
      end;

    procedure outerProc;
  end;

implementation

{ TOuterClass.TInnerClass }

procedure TOuterClass.TInnerClass.innerProc;
begin

end;

{ TOuterClass }

procedure TOuterClass.outerProc;
begin

end;

end.
