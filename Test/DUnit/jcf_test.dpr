program jcf_test;

uses
  Forms,
  GUITestRunner in '..\..\..\..\..\Delphi\DUnit\src\GUITestRunner.pas' {GUITestRunner},
  TestFramework in '..\..\..\..\..\Delphi\DUnit\src\TestFramework.pas',
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
  JcfMiscFunctions in '..\..\Utils\JcfMiscFunctions.pas',
  FileUtils in '..\..\Utils\FileUtils.pas',
  JCFLog in '..\..\Utils\JcfLog.pas',
  TestFileParse in 'TestFileParse.pas',
  VisitParseTree in '..\..\Process\VisitParseTree.pas',
  BaseVisitor in '..\..\Process\BaseVisitor.pas',
  VisitSetXY in '..\..\Process\VisitSetXY.pas',
  RemoveComment in '..\..\Process\Obfuscate\RemoveComment.pas',
  ParseTreeNodeType in '..\..\Parse\ParseTreeNodeType.pas',
  RemoveUnneededWhiteSpace in '..\..\Process\Obfuscate\RemoveUnneededWhiteSpace.pas',
  FixCase in '..\..\Process\Obfuscate\FixCase.pas',
  ReduceWhiteSpace in '..\..\Process\Obfuscate\ReduceWhiteSpace.pas',
  RemoveConsecutiveWhiteSpace in '..\..\Process\Obfuscate\RemoveConsecutiveWhiteSpace.pas',
  RemoveReturn in '..\..\Process\Obfuscate\RemoveReturn.pas',
  RebreakLines in '..\..\Process\Obfuscate\RebreakLines.pas',
  TestObfuscate in 'TestObfuscate.pas',
  SetUses in '..\..\Settings\SetUses.pas',
  JCFSetBase in '..\..\Settings\JCFSetBase.pas',
  JCFSettings in '..\..\Settings\JCFSettings.pas',
  SetAlign in '..\..\Settings\SetAlign.pas',
  SetAnyWordCaps in '..\..\Settings\SetAnyWordCaps.pas',
  SetCaps in '..\..\Settings\SetCaps.pas',
  SetClarify in '..\..\Settings\SetClarify.pas',
  SetFile in '..\..\Settings\SetFile.pas',
  SetIndent in '..\..\Settings\SetIndent.pas',
  SetLog in '..\..\Settings\SetLog.pas',
  SetObfuscate in '..\..\Settings\SetObfuscate.pas',
  SetReplace in '..\..\Settings\SetReplace.pas',
  SetReturns in '..\..\Settings\SetReturns.pas',
  SetSpaces in '..\..\Settings\SetSpaces.pas',
  SetUi in '..\..\Settings\SetUi.pas',
  SettingsStream in '..\..\Settings\Streams\SettingsStream.pas',
  RegistrySettings in '..\..\Settings\Streams\RegistrySettings.pas',
  VersionConsts in '..\..\VersionConsts.pas',
  Capitalisation in '..\..\Process\Capitalisation\Capitalisation.pas',
  SpecificWordCaps in '..\..\Process\Capitalisation\SpecificWordCaps.pas',
  WarnCaseNoElse in '..\..\Process\Warnings\WarnCaseNoElse.pas',
  Warning in '..\..\Process\Warnings\Warning.pas',
  WarnDestroy in '..\..\Process\Warnings\WarnDestroy.pas',
  WarnEmptyBlock in '..\..\Process\Warnings\WarnEmptyBlock.pas',
  WarnRealType in '..\..\Process\Warnings\WarnRealType.pas',
  WarnAssignToFunctionName in '..\..\Process\Warnings\WarnAssignToFunctionName.pas',
  TokenUtils in '..\..\Parse\TokenUtils.pas',
  TestWarnings in 'TestWarnings.pas',
  StringsWriter in '..\..\ReadWrite\StringsWriter.pas',
  StringsConverter in '..\..\ReadWrite\StringsConverter.pas',
  StringsReader in '..\..\ReadWrite\StringsReader.pas',
  TabToSpace in '..\..\Process\Spacing\TabToSpace.pas',
  SpaceToTab in '..\..\Process\Spacing\SpaceToTab.pas',
  Nesting in '..\..\Process\Nesting.pas',
  AllProcesses in '..\..\Process\AllProcesses.pas',
  RemoveBlankLine in '..\..\Process\Obfuscate\RemoveBlankLine.pas',
  NoReturnAfter in '..\..\Process\Spacing\NoReturnAfter.pas',
  NoSpaceBefore in '..\..\Process\Spacing\NoSpaceBefore.pas',
  TestSpacing in 'TestSpacing.pas',
  BaseTestProcess in 'BaseTestProcess.pas',
  TestConverter in 'TestConverter.pas',
  NoReturnBefore in '..\..\Process\Spacing\NoReturnBefore.pas',
  NoSpaceAfter in '..\..\Process\Spacing\NoSpaceAfter.pas',
  SingleSpaceBefore in '..\..\Process\Spacing\SingleSpaceBefore.pas',
  SingleSpaceAfter in '..\..\Process\Spacing\SingleSpaceAfter.pas',
  ReturnBefore in '..\..\Process\Spacing\ReturnBefore.pas',
  ReturnAfter in '..\..\Process\Spacing\ReturnAfter.pas',
  BlockStyles in '..\..\Process\Spacing\BlockStyles.pas',
  SwitchableVisitor in '..\..\Process\SwitchableVisitor.pas',
  FormatFlags in '..\..\Process\FormatFlags.pas',
  Indenter in '..\..\Process\Indent\Indenter.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
