program jcf_test;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is jcf_test, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

uses
  Forms,
  GUITestRunner {GUITestRunner},
  TestFramework,
  BuildTokenList in '..\..\..\Parse\BuildTokenList.pas',
  ParseError in '..\..\..\Parse\ParseError.pas',
  ParseTreeNode in '..\..\..\Parse\ParseTreeNode.pas',
  SourceToken in '..\..\..\Parse\SourceToken.pas',
  SourceTokenList in '..\..\..\Parse\SourceTokenList.pas',
  fShowParseTree in '..\..\..\Parse\UI\fShowParseTree.pas' {frmShowParseTree},
  Converter in '..\..\..\ReadWrite\Converter.pas',
  ConvertTypes in '..\..\..\ReadWrite\ConvertTypes.pas',
  FileConverter in '..\..\..\ReadWrite\FileConverter.pas',
  JcfMiscFunctions in '..\..\..\Utils\JcfMiscFunctions.pas',
  FileUtils in '..\..\..\Utils\FileUtils.pas',
  JcfLog in '..\..\..\Utils\JcfLog.pas',
  TestAdvanceTextPos in '..\TestAdvanceTextPos.pas',
  BaseVisitor in '..\..\..\Process\BaseVisitor.pas',
  VisitSetXY in '..\..\..\Process\VisitSetXY.pas',
  RemoveComment in '..\..\..\Process\Obfuscate\RemoveComment.pas',
  ParseTreeNodeType in '..\..\..\Parse\ParseTreeNodeType.pas',
  RemoveUnneededWhiteSpace in '..\..\..\Process\Obfuscate\RemoveUnneededWhiteSpace.pas',
  FixCase in '..\..\..\Process\Obfuscate\FixCase.pas',
  ReduceWhiteSpace in '..\..\..\Process\Obfuscate\ReduceWhiteSpace.pas',
  RemoveConsecutiveWhiteSpace in '..\..\..\Process\Obfuscate\RemoveConsecutiveWhiteSpace.pas',
  RemoveReturn in '..\..\..\Process\Obfuscate\RemoveReturn.pas',
  RebreakLines in '..\..\..\Process\Obfuscate\RebreakLines.pas',
  TestFullClarify in '..\TestFullClarify.pas',
  TestObfuscate in '..\TestObfuscate.pas',
  SetUses in '..\..\..\Settings\SetUses.pas',
  JCFSetBase in '..\..\..\Settings\JCFSetBase.pas',
  SetAlign in '..\..\..\Settings\SetAlign.pas',
  SetCaps in '..\..\..\Settings\SetCaps.pas',
  SetClarify in '..\..\..\Settings\SetClarify.pas',
  SetFile in '..\..\..\Settings\SetFile.pas',
  SetIndent in '..\..\..\Settings\SetIndent.pas',
  SetObfuscate in '..\..\..\Settings\SetObfuscate.pas',
  SetReplace in '..\..\..\Settings\SetReplace.pas',
  SetReturns in '..\..\..\Settings\SetReturns.pas',
  SetSpaces in '..\..\..\Settings\SetSpaces.pas',
  SettingsStream in '..\..\..\Settings\Streams\SettingsStream.pas',
  RegistrySettings in '..\..\..\Settings\Streams\RegistrySettings.pas',
  VersionConsts in '..\..\..\VersionConsts.pas',
  Capitalisation in '..\..\..\Process\Capitalisation\Capitalisation.pas',
  SpecificWordCaps in '..\..\..\Process\Capitalisation\SpecificWordCaps.pas',
  WarnCaseNoElse in '..\..\..\Process\Warnings\WarnCaseNoElse.pas',
  Warning in '..\..\..\Process\Warnings\Warning.pas',
  WarnDestroy in '..\..\..\Process\Warnings\WarnDestroy.pas',
  WarnEmptyBlock in '..\..\..\Process\Warnings\WarnEmptyBlock.pas',
  WarnRealType in '..\..\..\Process\Warnings\WarnRealType.pas',
  WarnAssignToFunctionName in '..\..\..\Process\Warnings\WarnAssignToFunctionName.pas',
  TokenUtils in '..\..\..\Parse\TokenUtils.pas',
  TestWarnings in '..\TestWarnings.pas',
  StringsConverter in '..\..\..\ReadWrite\StringsConverter.pas',
  TabToSpace in '..\..\..\Process\Spacing\TabToSpace.pas',
  SpaceToTab in '..\..\..\Process\Spacing\SpaceToTab.pas',
  Nesting in '..\..\..\Process\Nesting.pas',
  AllProcesses in '..\..\..\Process\AllProcesses.pas',
  RemoveBlankLine in '..\..\..\Process\Obfuscate\RemoveBlankLine.pas',
  NoReturnAfter in '..\..\..\Process\Returns\NoReturnAfter.pas',
  NoSpaceBefore in '..\..\..\Process\Spacing\NoSpaceBefore.pas',
  TestSpacing in '..\TestSpacing.pas',
  TestAlignment in '..\TestAlignment.pas',
  TestIfElseBreaks in '..\TestIfElseBreaks.pas',
  TestUsesReturns in '..\TestUsesReturns.pas',
  TestFindReplace in '..\TestFindReplace.pas',
  TestUsesFindReplace in '..\TestUsesFindReplace.pas',
  BaseTestProcess in '..\BaseTestProcess.pas',
  NoReturnBefore in '..\..\..\Process\Returns\NoReturnBefore.pas',
  NoSpaceAfter in '..\..\..\Process\Spacing\NoSpaceAfter.pas',
  SingleSpaceBefore in '..\..\..\Process\Spacing\SingleSpaceBefore.pas',
  SingleSpaceAfter in '..\..\..\Process\Spacing\SingleSpaceAfter.pas',
  ReturnBefore in '..\..\..\Process\Returns\ReturnBefore.pas',
  ReturnAfter in '..\..\..\Process\Returns\ReturnAfter.pas',
  BlockStyles in '..\..\..\Process\Returns\BlockStyles.pas',
  SwitchableVisitor in '..\..\..\Process\SwitchableVisitor.pas',
  FormatFlags in '..\..\..\Process\FormatFlags.pas',
  Indenter in '..\..\..\Process\Indent\Indenter.pas',
  VisitSetNesting in '..\..\..\Process\VisitSetNesting.pas',
  SpaceBeforeColon in '..\..\..\Process\Spacing\SpaceBeforeColon.pas',
  VisitStripEmptySpace in '..\..\..\Process\VisitStripEmptySpace.pas',
  PropertyOnOneLine in '..\..\..\Process\Returns\PropertyOnOneLine.pas',
  RemoveBlankLinesAfterProcHeader in '..\..\..\Process\Returns\RemoveBlankLinesAfterProcHeader.pas',
  RemoveBlankLinesInVars in '..\..\..\Process\Returns\RemoveBlankLinesInVars.pas',
  RemoveReturnsAfterBegin in '..\..\..\Process\Returns\RemoveReturnsAfterBegin.pas',
  RemoveReturnsBeforeEnd in '..\..\..\Process\Returns\RemoveReturnsBeforeEnd.pas',
  LongLineBreaker in '..\..\..\Process\Returns\LongLineBreaker.pas',
  IntList in '..\..\..\Utils\IntList.pas',
  ReturnChars in '..\..\..\Process\Returns\ReturnChars.pas',
  BasicStats in '..\..\..\Process\Info\BasicStats.pas',
  AlignBase in '..\..\..\Process\Align\AlignBase.pas',
  AlignConst in '..\..\..\Process\Align\AlignConst.pas',
  AlignAssign in '..\..\..\Process\Align\AlignAssign.pas',
  AlignVars in '..\..\..\Process\Align\AlignVars.pas',
  AlignTypedef in '..\..\..\Process\Align\AlignTypedef.pas',
  AlignComment in '..\..\..\Process\Align\AlignComment.pas',
  JcfRegistrySettings in '..\..\..\Settings\JcfRegistrySettings.pas',
  JCFSettings in '..\..\..\Settings\JCFSettings.pas',
  TestCommandLine in '..\TestCommandLine.pas',
  TestConstants in '..\TestConstants.pas',
  TestFile in '..\TestFile.pas',
  RemoveSpaceAtLineEnd in '..\..\..\Process\Spacing\RemoveSpaceAtLineEnd.pas',
  Tokens in '..\..\..\Parse\Tokens.pas',
  SetWordList in '..\..\..\Settings\SetWordList.pas',
  UnitNameCaps in '..\..\..\Process\Capitalisation\UnitNameCaps.pas',
  TestSelfParse in '..\TestSelfParse.pas',
  TestPreprocessorTokens in '..\TestPreprocessorTokens.pas',
  PreProcessorExpressionTokens in '..\..\..\Parse\PreProcessor\PreProcessorExpressionTokens.pas',
  PreProcessorExpressionParser in '..\..\..\Parse\PreProcessor\PreProcessorExpressionParser.pas',
  PreProcessorExpressionTokenise in '..\..\..\Parse\PreProcessor\PreProcessorExpressionTokenise.pas',
  JCFHelp in '..\..\..\Utils\JCFHelp.pas',
  SettingsTypes in '..\..\..\Settings\SettingsTypes.pas',
  SetPreProcessor in '..\..\..\Settings\SetPreProcessor.pas',
  FindReplace in '..\..\..\Process\Transform\FindReplace.pas',
  fJcfErrorDisplay in '..\..\..\Ui\fJcfErrorDisplay.pas' {ExceptionDialog},
  ReturnsAfterFinalEnd in '..\..\..\Process\Returns\ReturnsAfterFinalEnd.pas',
  UsesClauseRemove in '..\..\..\Process\Transform\UsesClauseRemove.pas',
  UsesClauseFindReplace in '..\..\..\Process\Transform\UsesClauseFindReplace.pas',
  UsesClauseInsert in '..\..\..\Process\Transform\UsesClauseInsert.pas',
  PreProcessorParseTree in '..\..\..\Parse\PreProcessor\PreProcessorParseTree.pas',
  RemoveEmptyComment in '..\..\..\Process\RemoveEmptyComment.pas',
  RemoveConsecutiveReturns in '..\..\..\Process\Returns\RemoveConsecutiveReturns.pas',
  TestRemoveComment in '..\TestRemoveComment.pas',
  TestMaxBlankLines in '..\TestMaxBlankLines.pas',
  TestTextAfterUnitEnd in '..\TestTextAfterUnitEnd.pas',
  TestCaseReturns in '..\TestCaseReturns.pas',
  TestIndentation in '..\TestIndentation.pas',
  MaxSpaces in '..\..\..\Process\Spacing\MaxSpaces.pas',
  TreeWalker in '..\..\..\Process\TreeWalker.pas',
  SetComments in '..\..\..\Settings\SetComments.pas',
  AddBeginEnd in '..\..\..\Process\Transform\AddBeginEnd.pas',
  AddBlockEndSemicolon in '..\..\..\Process\Transform\AddBlockEndSemicolon.pas',
  SetTransform in '..\..\..\Settings\SetTransform.pas',
  TestAddBeginEnd in '..\TestAddBeginEnd.pas',
  AlignField in '..\..\..\Process\Align\AlignField.pas',
  TestFormatPart in '..\TestFormatPart.pas',
  SortUses in '..\..\..\Process\Transform\SortUses.pas',
  TestSortUses in '..\TestSortUses.pas',
  BuildParseTree in '..\..\..\Parse\BuildParseTree.pas',
  SortUsesData in '..\..\..\Process\Transform\SortUsesData.pas',
  TestBreakConditionalCompilation in '..\TestBreakConditionalCompilation.pas',
  TestCapitalisation in '..\TestCapitalisation.pas',
  IdentifierCaps in '..\..\..\Process\Capitalisation\IdentifierCaps.pas',
  WarnUnusedParam in '..\..\..\Process\Warnings\WarnUnusedParam.pas',
  TestFileConverter in '..\TestFileConverter.pas',
  TestIndentElse in '..\TestIndentElse.pas',
  TestMiscFunctions in '..\TestMiscFunctions.pas',
  JcfFontSetFunctions in '..\..\..\Utils\JcfFontSetFunctions.pas',
  TestSpacingInsideBrackets in '..\TestSpacingInsideBrackets.pas',
  SetAsm in '..\..\..\Settings\SetAsm.pas',
  TestAsmOptionsCaps in '..\TestAsmOptionsCaps.pas',
  TestAsmOptionsBreaks in '..\TestAsmOptionsBreaks.pas',
  RemoveReturnsAfter in '..\..\..\Process\Returns\RemoveReturnsAfter.pas',
  TestAsmOptionsIndents in '..\TestAsmOptionsIndents.pas',
  IndentAsmParam in '..\..\..\Process\Indent\IndentAsmParam.pas',
  TestAsmOptionsIndentsBare in '..\TestAsmOptionsIndentsBare.pas',
  AsmKeywords in '..\..\..\Parse\AsmKeywords.pas',
  TestAsmParamKeywords in '..\TestAsmParamKeywords.pas',
  JcfUnicode in '..\..\..\Utils\JcfUnicode.pas',
  TestUnicodeFiles in '..\TestUnicodeFiles.pas',
  TestUnicodeFunctions in '..\TestUnicodeFunctions.pas',
  JcfUnicodeFiles in '..\..\..\Utils\JcfUnicodeFiles.pas',
  TestBreakAfterUses in '..\TestBreakAfterUses.pas',
  TestSpaceBeforeColon in '..\TestSpaceBeforeColon.pas',
  TestSpaceBeforeBrackets in '..\TestSpaceBeforeBrackets.pas',
  TestJcfStringUtils in '..\TestJcfStringUtils.pas',
  TestJcfUnicode in '..\TestJcfUnicode.pas',
  TestMoveSpaceToBeforeColon in '..\TestMoveSpaceToBeforeColon.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
