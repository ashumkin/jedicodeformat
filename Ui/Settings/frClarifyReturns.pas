{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frClarify.pas, released April 2000.
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

unit frClarifyReturns;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  JvEdit, JvTypedEdit, frmBaseSettingsFrame;

type
  TfClarifyReturns = class(TfrSettingsFrame)
    eNumReturnsAfterFinalEnd: TJvIntegerEdit;
    Label1: TLabel;
    rgReturnChars: TRadioGroup;
    GroupBox1: TGroupBox;
    cbRemoveProcDefReturns: TCheckBox;
    cbRemoveVarReturns: TCheckBox;
    cbRemoveBlockBlankLines: TCheckBox;
    cbRemoveExprReturns: TCheckBox;
    cbRemovePropertyReturns: TCheckBox;
    cbRemoveReturns: TCheckBox;
    cbRemoveBlankLinesAfterProcHeader: TCheckBox;
    cbRemoveVarBlankLines: TCheckBox;
    gbInsert: TGroupBox;
    cbUsesClauseOnePerLine: TCheckBox;
    cbInsertReturns: TCheckBox;
  private

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses Tokens, SettingsTypes, JcfSettings, SetReturns, JcfHelp;


constructor TfClarifyReturns.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_RETURNS;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyReturns.Read;
begin
  with FormatSettings.Returns do
  begin
    eNumReturnsAfterFinalEnd.Value := NumReturnsAfterFinalEnd;

    cbInsertReturns.Checked := AddGoodReturns;
    cbRemoveReturns.Checked := RemoveBadReturns;
    cbRemovePropertyReturns.Checked := RemovePropertyReturns;
    cbRemoveExprReturns.Checked := RemoveExpressionReturns;
    cbRemoveVarReturns.Checked := RemoveVarReturns;
    cbRemoveBlankLinesAfterProcHeader.Checked := RemoveProcHeaderBlankLines;

    cbRemoveProcDefReturns.Checked := RemoveProcedureDefReturns;

    cbRemoveVarBlankLines.Checked := RemoveVarBlankLines;
    cbRemoveBlockBlankLines.Checked := RemoveBlockBlankLines;
    cbUsesClauseOnePerLine.Checked := UsesClauseOnePerLine;

    rgReturnChars.ItemIndex := Ord(ReturnChars);
  end;
end;

procedure TfClarifyReturns.Write;
begin
  with FormatSettings.Returns do
  begin
    NumReturnsAfterFinalEnd := eNumReturnsAfterFinalEnd.Value;

    AddGoodReturns := cbInsertReturns.Checked;
    RemoveBadReturns := cbRemoveReturns.Checked;
    RemovePropertyReturns := cbRemovePropertyReturns.Checked;
    RemoveExpressionReturns := cbRemoveExprReturns.Checked;
    RemoveVarReturns := cbRemoveVarReturns.Checked;
    RemoveProcHeaderBlankLines := cbRemoveBlankLinesAfterProcHeader.Checked;

    RemoveProcedureDefReturns := cbRemoveProcDefReturns.Checked;

    RemoveVarBlankLines := cbRemoveVarBlankLines.Checked;
    RemoveBlockBlankLines := cbRemoveBlockBlankLines.Checked;
    UsesClauseOnePerLine := cbUsesClauseOnePerLine.Checked;

    ReturnChars := TReturnChars(rgReturnChars.ItemIndex);

  end;
end;

end.