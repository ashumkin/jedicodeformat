unit TestDelphiNetClass;

{
  AFS May 2005
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 Basic test of class heritage, visibility and uses in Delphi.NET
}

interface

uses
  Borland.Vcl.SysUtils,
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data;

type

  TMyForm = class(System.Windows.Forms.Form)
    TextBox: System.Windows.Forms.TextBox;
  end;

  TTestClass = class(TObject)
    strict private
      foo: integer;
    strict protected
      bar: integer;
    private
      fish: integer;
    protected
      spon: integer;
  end;

  TTestRecord = record
  strict private
    fNameValue : integer;
    function GetName: string;
  public
    NamePrefix : string;
    constructor Create(const psNameValue: integer);
    property Name : string read GetName;
  end;


implementation

{ TTestRecord }

constructor TTestRecord.Create(const psNameValue: integer);
begin
  fNameValue := psNameValue;
  NamePrefix := 'Test';
end;

function TTestRecord.GetName: string;
begin
  Inc(fNameValue) ;
  result := Format('%s %d',[NamePrefix, fNameValue]) ;
end;

end.
 