unit frClarifyIndent;

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

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  JvEdit, JvTypedEdit, frmBaseSettingsFrame;

type
  TfClarifyIndent = class(TfrSettingsFrame)
    Label2: TLabel;
    edtIndentSpaces: TJvIntegerEdit;
    cbIndentBeginEnd: TCheckBox;
    eIndentBeginEndSpaces: TJvIntegerEdit;
    cbHasFirstLevelIndent: TCheckBox;
    eFirstLevelIndent: TJvIntegerEdit;
    cbIndentGlobals: TCheckBox;
    cbIndentProcedures: TCheckBox;
    cbIndentClasses: TCheckBox;
    cbKeepWithInProc: TCheckBox;
    cbKeepWithInGlobals: TCheckBox;
    cbKeepWithInClassDef: TCheckBox;
    cbBorlandCaseIndent: TCheckBox;
    procedure cbIndentBeginEndClick(Sender: TObject);
    procedure cbHasFirstLevelIndentClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses JcfHelp, JcfSettings, SetIndent;

constructor TfClarifyIndent.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_INDENTATION;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyIndent.Read;
begin
  with FormatSettings.Indent do
  begin
    cbIndentGlobals.Checked    := IndentGlobals;
    cbIndentProcedures.Checked := IndentProcedures;
    cbIndentClasses.Checked    := IndentClasses;

    edtIndentSpaces.Value := IndentSpaces;
    cbIndentBeginEnd.Checked := IndentBeginEnd;
    eIndentBeginEndSpaces.Value := IndentBeginEndSpaces;

    cbHasFirstLevelIndent.Checked := HasFirstLevelIndent;
    eFirstLevelIndent.Value := FirstLevelIndent;

    cbKeepWithInProc.Checked := KeepCommentsWithCodeInProcs;
    cbKeepWithInGlobals.Checked := KeepCommentsWithCodeInGlobals;
    cbKeepWithInClassDef.Checked := KeepCommentsWithCodeInClassDef;
  end;

  cbIndentBeginEndClick(nil);
  cbHasFirstLevelIndentClick(nil);
end;

procedure TfClarifyIndent.Write;
begin

  with FormatSettings.Indent do
  begin
    IndentGlobals    := cbIndentGlobals.Checked;
    IndentProcedures := cbIndentProcedures.Checked;
    IndentClasses    := cbIndentClasses.Checked;

    IndentSpaces := edtIndentSpaces.Value;
    IndentBeginEnd := cbIndentBeginEnd.Checked;
    IndentBeginEndSpaces := eIndentBeginEndSpaces.Value;

    HasFirstLevelIndent := cbHasFirstLevelIndent.Checked;
    FirstLevelIndent := eFirstLevelIndent.Value;

    KeepCommentsWithCodeInProcs := cbKeepWithInProc.Checked;
    KeepCommentsWithCodeInGlobals := cbKeepWithInGlobals.Checked;
    KeepCommentsWithCodeInClassDef := cbKeepWithInClassDef.Checked;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifyIndent.cbIndentBeginEndClick(Sender: TObject);
begin
  eIndentBeginEndSpaces.Enabled := cbIndentBeginEnd.Checked;
end;

procedure TfClarifyIndent.cbHasFirstLevelIndentClick(Sender: TObject);
begin
  eFirstLevelIndent.Enabled := cbHasFirstLevelIndent.Checked;
end;

end.