program JcfNotepad;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfNotepad, released May 2003.
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
------------------------------------------------------------------------------*)
{*)}

uses
  Forms,
  frmJcfNotepad in 'frmJcfNotepad.pas' {fmJCFNotepad},
  Converter in '..\ReadWrite\Converter.pas',
  CodeReader in '..\ReadWrite\CodeReader.pas',
  CodeWriter in '..\ReadWrite\CodeWriter.pas',
  StringsReader in '..\ReadWrite\StringsReader.pas',
  StringsWriter in '..\ReadWrite\StringsWriter.pas',
  StringsConverter in '..\ReadWrite\StringsConverter.pas',
  ConvertTypes in '..\ReadWrite\ConvertTypes.pas',
  BuildParseTree in '..\Parse\BuildParseTree.pas',
  BuildTokenList in '..\Parse\BuildTokenList.pas',
  ParseError in '..\Parse\ParseError.pas',
  ParseTreeNode in '..\Parse\ParseTreeNode.pas',
  ParseTreeNodeType in '..\Parse\ParseTreeNodeType.pas',
  SourceToken in '..\Parse\SourceToken.pas',
  SourceTokenList in '..\Parse\SourceTokenList.pas',
  VisitSetXY in '..\Process\VisitSetXY.pas',
  BaseVisitor in '..\Process\BaseVisitor.pas',
  VisitParseTree in '..\Process\VisitParseTree.pas',
  JcfMiscFunctions in '..\Utils\JcfMiscFunctions.pas',
  FileUtils in '..\Utils\FileUtils.pas',
  JCFLog in '..\Utils\JcfLog.pas',
  fShowParseTree in '..\Parse\UI\fShowParseTree.pas' {frmShowParseTree},
  SetUses in '..\Settings\SetUses.pas',
  JCFSetBase in '..\Settings\JCFSetBase.pas',
  JCFSettings in '..\Settings\JCFSettings.pas',
  SetAlign in '..\Settings\SetAlign.pas',
  SetCaps in '..\Settings\SetCaps.pas',
  SetClarify in '..\Settings\SetClarify.pas',
  SetFile in '..\Settings\SetFile.pas',
  SetIndent in '..\Settings\SetIndent.pas',
  SetObfuscate in '..\Settings\SetObfuscate.pas',
  SetReplace in '..\Settings\SetReplace.pas',
  SetReturns in '..\Settings\SetReturns.pas',
  SetSpaces in '..\Settings\SetSpaces.pas',
  SettingsStream in '..\Settings\Streams\SettingsStream.pas',
  RegistrySettings in '..\Settings\Streams\RegistrySettings.pas',
  RemoveUnneededWhiteSpace in '..\Process\Obfuscate\RemoveUnneededWhiteSpace.pas',
  FixCase in '..\Process\Obfuscate\FixCase.pas',
  RebreakLines in '..\Process\Obfuscate\RebreakLines.pas',
  ReduceWhiteSpace in '..\Process\Obfuscate\ReduceWhiteSpace.pas',
  RemoveComment in '..\Process\Obfuscate\RemoveComment.pas',
  RemoveConsecutiveWhiteSpace in '..\Process\Obfuscate\RemoveConsecutiveWhiteSpace.pas',
  RemoveReturn in '..\Process\Obfuscate\RemoveReturn.pas',
  WarnRealType in '..\Process\Warnings\WarnRealType.pas',
  WarnAssignToFunctionName in '..\Process\Warnings\WarnAssignToFunctionName.pas',
  WarnCaseNoElse in '..\Process\Warnings\WarnCaseNoElse.pas',
  WarnDestroy in '..\Process\Warnings\WarnDestroy.pas',
  WarnEmptyBlock in '..\Process\Warnings\WarnEmptyBlock.pas',
  Warning in '..\Process\Warnings\Warning.pas',
  VersionConsts in '..\VersionConsts.pas',
  JcfRegistrySettings in '..\Settings\JcfRegistrySettings.pas',
  TokenUtils in '..\Parse\TokenUtils.pas',
  NoSpaceBefore in '..\Process\Spacing\NoSpaceBefore.pas',
  NoSpaceAfter in '..\Process\Spacing\NoSpaceAfter.pas',
  SingleSpaceAfter in '..\Process\Spacing\SingleSpaceAfter.pas',
  SingleSpaceBefore in '..\Process\Spacing\SingleSpaceBefore.pas',
  ReturnAfter in '..\Process\Returns\ReturnAfter.pas',
  Nesting in '..\Process\Nesting.pas',
  VisitSetNesting in '..\Process\VisitSetNesting.pas',
  ReturnBefore in '..\Process\Returns\ReturnBefore.pas',
  NoReturnAfter in '..\Process\Returns\NoReturnAfter.pas',
  NoReturnBefore in '..\Process\Returns\NoReturnBefore.pas',
  AllProcesses in '..\Process\AllProcesses.pas',
  RemoveBlankLine in '..\Process\Obfuscate\RemoveBlankLine.pas',
  BlockStyles in '..\Process\Returns\BlockStyles.pas',
  SwitchableVisitor in '..\Process\SwitchableVisitor.pas',
  FormatFlags in '..\Process\FormatFlags.pas',
  TabToSpace in '..\Process\Spacing\TabToSpace.pas',
  SpaceToTab in '..\Process\Spacing\SpaceToTab.pas',
  SpecificWordCaps in '..\Process\Capitalisation\SpecificWordCaps.pas',
  Capitalisation in '..\Process\Capitalisation\Capitalisation.pas',
  Indenter in '..\Process\Indent\Indenter.pas',
  PropertyOnOneLine in '..\Process\Returns\PropertyOnOneLine.pas',
  SpaceBeforeColon in '..\Process\Spacing\SpaceBeforeColon.pas',
  VisitStripEmptySpace in '..\Process\VisitStripEmptySpace.pas',
  RemoveBlankLinesAfterProcHeader in '..\Process\Returns\RemoveBlankLinesAfterProcHeader.pas',
  RemoveBlankLinesInVars in '..\Process\Returns\RemoveBlankLinesInVars.pas',
  ReturnChars in '..\Process\Returns\ReturnChars.pas',
  RemoveReturnsBeforeEnd in '..\Process\Returns\RemoveReturnsBeforeEnd.pas',
  RemoveReturnsAfterBegin in '..\Process\Returns\RemoveReturnsAfterBegin.pas',
  LongLineBreaker in '..\Process\Returns\LongLineBreaker.pas',
  IntList in '..\Utils\IntList.pas',
  BasicStats in '..\Process\Info\BasicStats.pas',
  AlignConst in '..\Process\Align\AlignConst.pas',
  AlignBase in '..\Process\Align\AlignBase.pas',
  AlignAssign in '..\Process\Align\AlignAssign.pas',
  AlignVars in '..\Process\Align\AlignVars.pas',
  AlignTypedef in '..\Process\Align\AlignTypedef.pas',
  AlignComment in '..\Process\Align\AlignComment.pas',
  JCFDropTarget in '..\Utils\DragDrop\JCFDropTarget.pas',
  frDrop in '..\Utils\DragDrop\frDrop.pas' {FrameDrop: TFrame},
  JCFHelp in '..\Utils\JCFHelp.pas',
  fAbout in '..\Ui\fAbout.pas' {frmAboutBox},
  frmBaseSettingsFrame in '..\Ui\Settings\frmBaseSettingsFrame.pas' {frSettingsFrame: TFrame},
  fAllSettings in '..\Ui\fAllSettings.pas' {FormAllSettings},
  frFiles in '..\Ui\Settings\frFiles.pas' {fFiles: TFrame},
  frObfuscateSettings in '..\Ui\Settings\frObfuscateSettings.pas' {fObfuscateSettings: TFrame},
  frUses in '..\Ui\Settings\frUses.pas' {fUses: TFrame},
  frAnyCapsSettings in '..\Ui\Settings\frAnyCapsSettings.pas' {frAnyCapsSettings: TFrame},
  frBasicSettings in '..\Ui\Settings\frBasicSettings.pas' {frBasic: TFrame},
  frClarify in '..\Ui\Settings\frClarify.pas' {fClarify: TFrame},
  frClarifyAlign in '..\Ui\Settings\frClarifyAlign.pas' {fClarifyAlign: TFrame},
  frClarifyBlocks in '..\Ui\Settings\frClarifyBlocks.pas' {fClarifyBlocks: TFrame},
  frClarifyIndent in '..\Ui\Settings\frClarifyIndent.pas' {fClarifyIndent: TFrame},
  frClarifyReturns in '..\Ui\Settings\frClarifyReturns.pas' {fClarifyReturns: TFrame},
  frClarifySpaces in '..\Ui\Settings\frClarifySpaces.pas' {fClarifySpaces: TFrame},
  frReplace in '..\Ui\Settings\frReplace.pas' {fReplace: TFrame},
  frReservedCapsSettings in '..\Ui\Settings\frReservedCapsSettings.pas' {frReservedCapsSettings: TFrame},
  frClarifyLongLineBreaker in '..\Ui\Settings\frClarifyLongLineBreaker.pas' {fClarifyLongLineBreaker: TFrame},
  fRegistrySettings in '..\Ui\fRegistrySettings.pas' {fmRegistrySettings},
  MozComment in '..\Process\Onceoffs\MozComment.pas',
  Tokens in '..\Parse\Tokens.pas',
  SettingsTypes in '..\Settings\SettingsTypes.pas',
  SetWordList in '..\Settings\SetWordList.pas',
  UnitNameCaps in '..\Process\Capitalisation\UnitNameCaps.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmJCFNotepad, fmJCFNotepad);
  Application.Run;
end.