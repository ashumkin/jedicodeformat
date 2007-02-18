library TestDelphiNetLibrary;

{
 This unit compiles but is not semantically meaningfull
 it is test cases for the code formatting utility

 A delphi.net library with assembly attributes
}

{%DelphiDotNetAssemblyCompiler 'c:\programme\gemeinsame dateien\borland shared\bds\shared assemblies\2.0\Borland.Delphi.dll'}
{%DelphiDotNetAssemblyCompiler '\microsoft.net\framework\v1.1.4322\System.EnterpriseServices.dll'}

uses
  System.Reflection,
  System.EnterpriseServices,
  System.Runtime.InteropServices;

[assembly: ApplicationName('Delphi8ESFixInterfaceDemo')]
[assembly: ApplicationActivation(ActivationOption.Server)] 
[assembly: ApplicationAccessControl(Value=True,
  Authentication=AuthenticationOption.Connect,
  ImpersonationLevel=ImpersonationLevelOption.Identify,
  AccessChecksLevel=AccessChecksLevelOption.ApplicationComponent)]
[assembly: DescriptionAttribute('Delphi 8 Interface .NET Enterprise Services-Object')]
[assembly: SecurityRole('Benutzer', SetEveryoneAccess = True)]

// STRG+UMSCHALT+G legt neue GUID für die Assembly fest
[assembly: Guid('854A605E-3F7E-4625-A79B-DA6E074269BA')]

[assembly: AssemblyTitle('Delphi8ESFix')]
[assembly: AssemblyDescription('Delphi 8 FixInterface .NET Enterprise Service')]
[assembly: AssemblyVersion('1.1.0.0')]
[assembly: AssemblyDelaySign(False)]
[assembly: AssemblyKeyFile('ES.snk')]
[assembly: AssemblyKeyName('')]

type
  IOSDelphi8ESObj = interface
    function DoWork(sInput: String): String;
  end;

  [TransactionAttribute(TransactionOption.NotSupported),
   ConstructionEnabled(Default='Delphi 8 FixInterface'),
   JustInTimeActivation(True),
   EventTrackingEnabled(True),
   DescriptionAttribute('Delphi 8 FixInterface .NET Enterprise Services-Objekt'),
   Guid('356C3A64-88B3-4AA6-B644-D89D103B2F41'),
   ObjectPooling(MinPoolSize=2, MaxPoolSize=5),
   ClassInterface(ClassInterfaceType.None)]
  TOSDelphi8ESObj = class(ServicedComponent, IOSDelphi8ESObj)
  private
    FConstructString: String;
  protected
    procedure Construct(constructString: String); override;
  public
    function DoWork(sInput: String): String;
  end;

{ TOSDelphi8ESObj }

procedure TOSDelphi8ESObj.Construct(constructString: String);
begin
  inherited;
  FConstructString := constructString;
end;

function TOSDelphi8ESObj.DoWork(sInput: String): String;
begin
  Result := sInput + ': ' + FConstructString;
  ContextUtil.SetComplete;
end;

begin
end.

