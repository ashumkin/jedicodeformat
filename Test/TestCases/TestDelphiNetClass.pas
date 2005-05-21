unit TestDelphiNetClass;

{
  AFS May 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Basic test of class heritage, visibility and uses in Delphi.NET
}

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
 