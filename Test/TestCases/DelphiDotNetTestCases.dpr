program DelphiDotNetTestCases;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Data.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.XML.dll'}

uses
  System.Reflection,
  System.Runtime.CompilerServices,
  System.Runtime.InteropServices,
  System.Windows.Forms,
  TestDelphiNetAmpersandMethod in 'TestDelphiNetAmpersandMethod.pas',
  TestDelphiNetAttributes in 'TestDelphiNetAttributes.pas',
  TestDelphiNetClass in 'TestDelphiNetClass.pas' {TestDelphiNetClass.TMyForm: System.Windows.Forms.Form},
  TestDelphiNetClassVar in 'TestDelphiNetClassVar.pas',
  TestDelphiNetConst in 'TestDelphiNetConst.pas',
  TestDelphiNetDottedType in 'TestDelphiNetDottedType.pas',
  TestDelphiNetDynamicArray in 'TestDelphiNetDynamicArray.pas',
  TestDelphiNetFinalMethod in 'TestDelphiNetFinalMethod.pas',
  TestDelphiNetHelperClass in 'TestDelphiNetHelperClass.pas',
  TestDelphiNetKeywords in 'TestDelphiNetKeywords.pas',
  TestDelphiNetMulticast in 'TestDelphiNetMulticast.pas',
  TestDelphiNetNestedType2 in 'TestDelphiNetNestedType2.pas',
  TestDelphiNetNestedType in 'TestDelphiNetNestedType.pas',
  TestDelphiNetOperatorOverload in 'TestDelphiNetOperatorOverload.pas',
  TestDelphiNetRecordForward in 'TestDelphiNetRecordForward.pas',
  TestDelphiNetRecordProcs in 'TestDelphiNetRecordProcs.pas',
  TestDelphiNetSealedClass in 'TestDelphiNetSealedClass.pas',
  TestDelphiNetStatic in 'TestDelphiNetStatic.pas',
  TestDelphiNetUses in 'TestDelphiNetUses.pas',
  TestDotNetForm1 in 'TestDotNetForm1.pas' {TestDotNetForm1.TWinForm: System.Windows.Forms.Form},
  TestLabelKeyword in 'TestLabelKeyword.pas',
  TestDelphiNetUnsafe in 'TestDelphiNetUnsafe.pas',
  TestDelphiNetRecordClassVars in 'TestDelphiNetRecordClassVars.pas';

{$R *.res}

{$REGION 'Program/Assembly Information'}
//
// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyDescription('')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('')]
[assembly: AssemblyProduct('')]
[assembly: AssemblyCopyright('')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

// The Delphi compiler controls the AssemblyTitleAttribute via the ExeDescription.
// You can set this in the IDE via the Project Options.
// Manually setting the AssemblyTitle attribute below will override the IDE
// setting.
// [assembly: AssemblyTitle('')]


//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Revision and Build Numbers 
// by using the '*' as shown below:

[assembly: AssemblyVersion('1.0.*')]

//
// In order to sign your assembly you must specify a key to use. Refer to the 
// Microsoft .NET Framework documentation for more information on assembly signing.
//
// Use the attributes below to control which key is used for signing. 
//
// Notes: 
//   (*) If no key is specified, the assembly is not signed.
//   (*) KeyName refers to a key that has been installed in the Crypto Service
//       Provider (CSP) on your machine. KeyFile refers to a file which contains
//       a key.
//   (*) If the KeyFile and the KeyName values are both specified, the 
//       following processing occurs:
//       (1) If the KeyName can be found in the CSP, that key is used.
//       (2) If the KeyName does not exist and the KeyFile does exist, the key 
//           in the KeyFile is installed into the CSP and used.
//   (*) In order to create a KeyFile, you can use the sn.exe (Strong Name) utility.
//       When specifying the KeyFile, the location of the KeyFile should be
//       relative to the project output directory. For example, if your KeyFile is
//       located in the project directory, you would specify the AssemblyKeyFile 
//       attribute as [assembly: AssemblyKeyFile('mykey.snk')], provided your output
//       directory is the project directory (the default).
//   (*) Delay Signing is an advanced option - see the Microsoft .NET Framework
//       documentation for more information on this.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]


//
// Use the attributes below to control the COM visibility of your assembly. By
// default the entire assembly is visible to COM. Setting ComVisible to false
// is the recommended default for your assembly. To then expose a class and interface
// to COM set ComVisible to true on each one. It is also recommended to add a
// Guid attribute.
//

[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]
{$ENDREGION}

[STAThread]
begin
  Application.Run(TWinForm.Create);
end.
