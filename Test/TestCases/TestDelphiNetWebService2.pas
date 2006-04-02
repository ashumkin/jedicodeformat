unit TestDelphiNetWebService2;

{ AFS March 2006
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility }

interface

uses
  System.Collections, System.ComponentModel,
  System.Data, System.Diagnostics, System.Web,
  System.Xml.Serialization,
  System.Web.Services;

type
  /// <summary>
  /// Summary description for WebService1.
  /// </summary>
  [WebService(Namespace='http://example.com/JCFTest/')]
  TWebService1 = class(System.Web.Services.WebService)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: IContainer;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(disposing: boolean); override;
  private
    { Private Declarations }
  public
    constructor Create;
    // Sample Web Service Method
    [WebMethod][result: XMLAttribute]
    function HelloWorld: string;
    [WebMethod][result: XmlElement]
    function Introduce([XMLAttribute]MySelf: String): String;
  end;

implementation

{$REGION 'Designer Managed Code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWebService1.InitializeComponent;
begin

end;
function TWebService1.Introduce(MySelf: String): String;
begin
  Result := 'Hello ' + MySelf;
end;

{$ENDREGION}

constructor TWebService1.Create;
begin
  inherited;
  //
  // Required for Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
end;

/// <summary>
/// Clean up any resources being used.
/// </summary>
procedure TWebService1.Dispose(disposing: boolean);
begin
  if disposing and (components <> nil) then
    components.Dispose;
  inherited Dispose(disposing);
end;

// Sample Web Service Method
// The following method is provided to allow for testing a new web service.
function TWebService1.HelloWorld: string;
begin
  Result := 'Hello World';
end;

end.

