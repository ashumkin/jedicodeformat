unit TestDelphiNetNestedType2;

interface

type
  TOuterClass = class
  private
    myField: Integer;
  public
    type
      TInnerClass = class
      private
          type
            TInner2Class = class
            public
              myInnerField: Integer;
              procedure innerProc;
            end;

      public
        myInnerField: Integer;
        procedure innerProc;
      end;

    procedure outerProc;
  end;

implementation

{ TOuterClass.TInnerClass }

{ TOuterClass }

procedure TOuterClass.outerProc;
begin

end;

procedure TOuterClass.TInnerClass.innerProc;
begin

end;


{ TOuterClass.TInnerClass.TInner2Class }

procedure TOuterClass.TInnerClass.TInner2Class.innerProc;
begin

end;

end.
