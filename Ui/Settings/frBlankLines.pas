{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frBlankLines.pas, released Nov 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
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
unit frBlankLines;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  JvEdit, JvTypedEdit, frmBaseSettingsFrame;


type
  TfBlankLines = class(TfrSettingsFrame)
    Label1: TLabel;
    eNumReturnsAfterFinalEnd: TJvIntegerEdit;
    cbRemoveConsecutiveBlankLines: TCheckBox;
    edtMaxConsecutiveBlankLines: TJvIntegerEdit;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    cbRemoveBlockBlankLines: TCheckBox;
    cbRemoveBlankLinesAfterProcHeader: TCheckBox;
    cbRemoveVarBlankLines: TCheckBox;
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$R *.dfm}

uses
  { delphi }
  Math,
  { local }
  Tokens, SettingsTypes, JcfSettings, SetReturns, JcfHelp;

constructor TfBlankLines.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_BLANK_LINES;
end;

procedure TfBlankLines.Read;
begin
  with FormatSettings.Returns do
  begin
    cbRemoveVarBlankLines.Checked := RemoveVarBlankLines;
    cbRemoveBlankLinesAfterProcHeader.Checked := RemoveProcHeaderBlankLines;
    cbRemoveBlockBlankLines.Checked := RemoveBlockBlankLines;

    eNumReturnsAfterFinalEnd.Value := NumReturnsAfterFinalEnd;

    cbRemoveConsecutiveBlankLines.Checked := RemoveConsecutiveBlankLines;
    edtMaxConsecutiveBlankLines.Value := MaxConsecutiveBlankLines;
  end;
end;

procedure TfBlankLines.Write;
begin
  with FormatSettings.Returns do
  begin
    RemoveVarBlankLines := cbRemoveVarBlankLines.Checked;
    RemoveProcHeaderBlankLines := cbRemoveBlankLinesAfterProcHeader.Checked;
    RemoveBlockBlankLines := cbRemoveBlockBlankLines.Checked;

    NumReturnsAfterFinalEnd := eNumReturnsAfterFinalEnd.Value;

    RemoveConsecutiveBlankLines := cbRemoveConsecutiveBlankLines.Checked;
    // this value is always at least 2
    MaxConsecutiveBlankLines := Max(edtMaxConsecutiveBlankLines.Value, 2);
  end;
end;

end.
