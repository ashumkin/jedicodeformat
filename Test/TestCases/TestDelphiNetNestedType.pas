unit TestDelphiNetNestedType;

{ This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Simple test of nested types in Delphi.net
}
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
