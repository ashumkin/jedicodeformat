unit SetReturns;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SetReturns.pas, released April 2000.
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

{ mostly spacing and line breaking +options }

interface

uses JCFSetBase, SettingsTypes, SettingsStream;

type
  { rebreak lines has three modes:
    - off, don't rebreak lines
    - only if you can find a good spot to do it
     yes (if there is any spot to do it)
  }
  TWhenToRebreakLines = (rbOff, rbOnlyIfGood, rbUsually);

  TSetReturns = class(TSetBase)
  private
    { line breaking }
    feRebreakLines: TWhenToRebreakLines;
    fiMaxLineLength: integer;

    { return removal and adding }
    fbRemoveBadReturns: Boolean;
    fbAddGoodReturns: Boolean;
    fbUsesClauseOnePerLine: boolean;

    fbRemoveExpressionReturns: Boolean;
    fbRemoveVarReturns: Boolean;
    fbRemovePropertyReturns: Boolean;
    fbRemoveProcedureDefReturns: Boolean;

    fbRemoveBlockBlankLines: boolean;
    fbRemoveVarBlankLines: boolean;
    fbRemoveProcHeaderBlankLines: boolean;

    fiNumReturnsAfterFinalEnd: integer;

    { returns on blocks }
    feBlockStyle, feBlockBeginStyle: TBlockNewLineStyle;
    feLabelStyle, feLabelBeginStyle: TBlockNewLineStyle;
    feCaseLabelStyle: TBlockNewLineStyle;
    feEndElseStyle: TBlockNewLineStyle;
    feElseIfStyle: TBlockNewLineStyle;

    feReturnChars: TReturnChars;

    fbRemoveConsecutiveReturns: Boolean;
    fiMaxConsecutiveReturns: integer;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property RebreakLines: TWhenToRebreakLines read feRebreakLines write feRebreakLines;
    property MaxLineLength: integer read fiMaxLineLength write fiMaxLineLength;

    property NumReturnsAfterFinalEnd: integer read fiNumReturnsAfterFinalEnd
      write fiNumReturnsAfterFinalEnd;

    property RemoveBadReturns: boolean read fbRemoveBadReturns write fbRemoveBadReturns;
    property AddGoodReturns: boolean read fbAddGoodReturns write fbAddGoodReturns;
    property UsesClauseOnePerLine: Boolean read fbUsesClauseOnePerLine write fbUsesClauseOnePerLine;

    property RemoveExpressionReturns: boolean read fbRemoveExpressionReturns write fbRemoveExpressionReturns;
    property RemoveVarReturns: boolean read fbRemoveVarReturns write fbRemoveVarReturns;
    property RemovePropertyReturns: boolean read fbRemovePropertyReturns write fbRemovePropertyReturns;
    property RemoveProcedureDefReturns: Boolean read fbRemoveProcedureDefReturns
      write fbRemoveProcedureDefReturns;

    property RemoveBlockBlankLines: boolean read fbRemoveBlockBlankLines write fbRemoveBlockBlankLines;
    property RemoveVarBlankLines: boolean read fbRemoveVarBlankLines write fbRemoveVarBlankLines;
    property RemoveProcHeaderBlankLines: boolean read fbRemoveProcHeaderBlankLines
      write fbRemoveProcHeaderBlankLines;


    property BlockStyle: TBlockNewLineStyle read feBlockStyle write feBlockStyle;
    property BlockBeginStyle: TBlockNewLineStyle read feBlockBeginStyle
      write feBlockBeginStyle;
    property LabelStyle: TBlockNewLineStyle read feLabelStyle write feLabelStyle;
    property LabelBeginStyle: TBlockNewLineStyle read feLabelBeginStyle
      write feLabelBeginStyle;
    property CaseLabelStyle: TBlockNewLineStyle read feCaseLabelStyle write feCaseLabelStyle;

    property EndElseStyle: TBlockNewLineStyle read feEndElseStyle write feEndElseStyle;
    property ElseIfStyle: TBlockNewLineStyle read feElseIfStyle write feElseIfStyle;

    property ReturnChars: TReturnChars read  feReturnChars write feReturnChars;

    property RemoveConsecutiveReturns: Boolean read fbRemoveConsecutiveReturns write fbRemoveConsecutiveReturns;
    property MaxConsecutiveReturns: integer read fiMaxConsecutiveReturns write fiMaxConsecutiveReturns;

  end;

implementation

const
  REG_WHEN_REBREAK_LINES   = 'WhenRebreakLines';
  REG_MAX_LINE_LENGTH = 'MaxLineLength';

  REG_NUM_RETURNS_AFTER_FINAL_END = 'NumReturnsAfterFinalEnd';

  REG_ALIGN_ASSIGN = 'AlignAssign';

  REG_REMOVE_BAD_RETURNS = 'RemoveBadReturns';
  REG_ADD_GOOD_RETURNS = 'AddGoodReturns';
  REG_USES_ONE_PER_LINE = 'UsesOnePerLine';

  REG_REMOVE_EXPRESSION_RETURNS = 'RemoveExpressionReturns';
  REG_REMOVE_VAR_RETURNS = 'RemoveVarReturns';
  REG_REMOVE_PROC_HEADER_BLANK_LINES = 'RemoveProcHeaderBlankLines';
  REG_REMOVE_PROPERTY_RETURNS = 'NoReturnsInProperty';
  REG_REMOVE_PROCEDURE_DEF_RETURNS = 'RemoveProcedureDefReturns';

  REG_REMOVE_BLOCK_BLANK_LINES = 'RemoveReturns';
  REG_REMOVE_VAR_BLANK_LINES = 'RemoveVarBlankLines';

  { block line breaking styles }
  REG_BLOCK_STYLE       = 'Block';
  REG_BLOCK_BEGIN_STYLE = 'BlockBegin';
  REG_LABEL_STYLE       = 'Label';
  REG_LABEL_BEGIN_STYLE = 'LabelBegin';
  REG_CASE_LABEL_STYLE  = 'CaseLabel';
  REG_END_ELSE_STYLE    = 'EndElse';
  REG_ELSE_IF_STYLE     = 'ElseIf';

  REG_RETURN_CHARS = 'ReturnChars';

  REG_REMOVE_CONSECUTIVE_RETURNS = 'RemoveConsecutiveReturns';
  REG_MAX_CONSECUTIVE_RETURNS = 'MaxConsecutiveReturns';

constructor TSetReturns.Create;
begin
  inherited;
  SetSection('Returns');
end;

procedure TSetReturns.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feRebreakLines  := TWhenToRebreakLines(pcStream.Read(REG_WHEN_REBREAK_LINES, Ord(rbOnlyIfGood)));
  fiMaxLineLength := pcStream.Read(REG_MAX_LINE_LENGTH, 90);

  fiNumReturnsAfterFinalEnd := pcStream.Read(REG_NUM_RETURNS_AFTER_FINAL_END, 1);

  fbRemoveBadReturns := pcStream.Read(REG_REMOVE_BAD_RETURNS, True);
  fbAddGoodReturns := pcStream.Read(REG_ADD_GOOD_RETURNS, True);
  fbUsesClauseOnePerLine := pcStream.Read(REG_USES_ONE_PER_LINE, False);

  fbRemoveExpressionReturns := pcStream.Read(REG_REMOVE_EXPRESSION_RETURNS, False);
  fbRemoveVarReturns := pcStream.Read(REG_REMOVE_VAR_RETURNS, True);
  fbRemovePropertyReturns := pcStream.Read(REG_REMOVE_PROPERTY_RETURNS, True);
  fbRemoveProcedureDefReturns := pcStream.Read(REG_REMOVE_PROCEDURE_DEF_RETURNS, False);

  fbRemoveBlockBlankLines := pcStream.Read(REG_REMOVE_BLOCK_BLANK_LINES, True);
  fbRemoveVarBlankLines := pcStream.Read(REG_REMOVE_VAR_BLANK_LINES, False);
  fbRemoveProcHeaderBlankLines := pcStream.Read(REG_REMOVE_PROC_HEADER_BLANK_LINES, True);

  feBlockStyle      := TBlockNewLineStyle(pcStream.Read(REG_BLOCK_STYLE, Ord(eLeave)));
  feBlockBeginStyle := TBlockNewLineStyle(pcStream.Read(REG_BLOCK_BEGIN_STYLE, Ord(eLeave)));
  feLabelStyle      := TBlockNewLineStyle(pcStream.Read(REG_LABEL_STYLE, Ord(eLeave)));
  feLabelBeginStyle := TBlockNewLineStyle(pcStream.Read(REG_LABEL_BEGIN_STYLE, Ord(eLeave)));
  feCaseLabelStyle  := TBlockNewLineStyle(pcStream.Read(REG_CASE_LABEL_STYLE, Ord(eLeave)));
  feEndElseStyle    := TBlockNewLineStyle(pcStream.Read(REG_END_ELSE_STYLE, Ord(eLeave)));
  feElseIfStyle     := TBlockNewLineStyle(pcStream.Read(REG_ELSE_IF_STYLE, Ord(eNever)));

  feReturnChars := TReturnChars(pcStream.Read(REG_RETURN_CHARS, Ord(rcLeaveAsIs)));

  fbRemoveConsecutiveReturns := pcStream.Read(REG_REMOVE_CONSECUTIVE_RETURNS, True);
  fiMaxConsecutiveReturns := pcStream.Read(REG_MAX_CONSECUTIVE_RETURNS, 4);
end;

procedure TSetReturns.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_WHEN_REBREAK_LINES, Ord(feRebreakLines));
  pcOut.Write(REG_MAX_LINE_LENGTH, fiMaxLineLength);

  pcOut.Write(REG_NUM_RETURNS_AFTER_FINAL_END, fiNumReturnsAfterFinalEnd);

  pcOut.Write(REG_REMOVE_BAD_RETURNS, fbRemoveBadReturns);
  pcOut.Write(REG_ADD_GOOD_RETURNS, fbAddGoodReturns);
  pcOut.Write(REG_USES_ONE_PER_LINE, UsesClauseOnePerLine);

  pcOut.Write(REG_REMOVE_EXPRESSION_RETURNS, fbRemoveExpressionReturns);
  pcOut.Write(REG_REMOVE_VAR_RETURNS, fbRemoveVarReturns);
  pcOut.Write(REG_REMOVE_PROPERTY_RETURNS, fbRemovePropertyReturns);
  pcOut.Write(REG_REMOVE_PROCEDURE_DEF_RETURNS, fbRemoveProcedureDefReturns);

  pcOut.Write(REG_REMOVE_BLOCK_BLANK_LINES, fbRemoveBlockBlankLines);
  pcOut.Write(REG_REMOVE_VAR_BLANK_LINES, fbRemoveVarBlankLines);
  pcOut.Write(REG_REMOVE_PROC_HEADER_BLANK_LINES, fbRemoveProcHeaderBlankLines);

  pcOut.Write(REG_BLOCK_STYLE, Ord(feBlockStyle));
  pcOut.Write(REG_BLOCK_BEGIN_STYLE, Ord(feBlockBeginStyle));
  pcOut.Write(REG_LABEL_STYLE, Ord(feLabelStyle));
  pcOut.Write(REG_LABEL_BEGIN_STYLE, Ord(feLabelBeginStyle));
  pcOut.Write(REG_CASE_LABEL_STYLE, Ord(feCaseLabelStyle));

  pcOut.Write(REG_END_ELSE_STYLE, Ord(feEndElseStyle));
  pcOut.Write(REG_ELSE_IF_STYLE, Ord(feElseIfStyle));

  pcOut.Write(REG_RETURN_CHARS, Ord(feReturnChars));

  pcOut.Write(REG_REMOVE_CONSECUTIVE_RETURNS, fbRemoveConsecutiveReturns);
  pcOut.Write(REG_MAX_CONSECUTIVE_RETURNS, fiMaxConsecutiveReturns);
end;

end.