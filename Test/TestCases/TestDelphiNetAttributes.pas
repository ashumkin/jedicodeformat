unit TestDelphiNetAttributes;

{
 Basic test of attributes in Delphi.NET
}
interface
uses
  Classes;

type

  [Serializable]
  TContainer = class(TObject)
  public
  end;
  [Serializable]
  TContainer2 = class(TObject)
  public
  end;
  TContainer3 = class(TObject)
  public
  end;
  [Serializable]
  TContainer4 = class(TObject)
  public
  end;


  [Serializable]
  TJCFConfiguration = class(TObject)
  public
    CommandlineParams: string;
    Style: integer;

    a,b, c: string;

    [NonSerialized]
    foo, bar: integer;
  end;

  TContainer6 = class(TObject)
  public
    [NonSerialized]
    foo, bar: integer;

    foo2, bar2: integer;

    [NonSerialized]
    foo3, bar3: integer;
    foo4, bar4: integer;
  end;

implementation

end.
