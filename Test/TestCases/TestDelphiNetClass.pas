unit TestDelphiNetClass;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data;

type

  TMyForm = class(System.Windows.Forms.Form)
    TextBox: System.Windows.Forms.TextBox;
  end;

  TTest = class(TObject)
    strict private
      foo: integer;
    strict protected
      bar: integer;
    private
      fish: integer;
    protected
      spon: integer;
  end;


implementation

end.
 