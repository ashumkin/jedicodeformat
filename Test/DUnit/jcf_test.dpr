program jcf_test;

uses
  Forms,
  TestFramework in '..\..\..\..\..\DUnit\src\TestFramework.pas',
  GUITestRunner in '..\..\..\..\..\DUnit\src\GUITestRunner.pas' {GUITestRunner},
  WordMap in '..\..\Parse\WordMap.pas',
  BuildParseTree in '..\..\Parse\BuildParseTree.pas',
  BuildTokenList in '..\..\Parse\BuildTokenList.pas',
  ParseError in '..\..\Parse\ParseError.pas',
  ParseTreeNode in '..\..\Parse\ParseTreeNode.pas',
  SourceToken in '..\..\Parse\SourceToken.pas',
  SourceTokenList in '..\..\Parse\SourceTokenList.pas',
  TokenType in '..\..\Parse\TokenType.pas',
  fShowParseTree in '..\..\Parse\UI\fShowParseTree.pas' {frmShowParseTree},
  FileWriter in '..\..\ReadWrite\FileWriter.pas',
  CodeReader in '..\..\ReadWrite\CodeReader.pas',
  CodeWriter in '..\..\ReadWrite\CodeWriter.pas',
  Converter in '..\..\ReadWrite\Converter.pas',
  ConvertTypes in '..\..\ReadWrite\ConvertTypes.pas',
  FileConverter in '..\..\ReadWrite\FileConverter.pas',
  FileReader in '..\..\ReadWrite\FileReader.pas',
  MiscFunctions in '..\..\Utils\MiscFunctions.pas',
  FileUtils in '..\..\Utils\FileUtils.pas',
  JCFLog in '..\..\Utils\JcfLog.pas',
  TestFileParse in 'TestFileParse.pas',
  VisitParseTree in '..\..\Process\VisitParseTree.pas',
  BaseVisitor in '..\..\Process\BaseVisitor.pas',
  VisitSetXY in '..\..\Process\VisitSetXY.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
