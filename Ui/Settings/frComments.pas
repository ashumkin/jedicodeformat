unit frComments;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frComments.pas, released Nov 2003.
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

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  { local }
  frmBaseSettingsFrame, StdCtrls;

type
  TfComments = class(TfrSettingsFrame)
    cbRemoveEmptyDoubleSlashComments: TCheckBox;
    cbRemoveEmptyCurlyBraceComments: TCheckBox;
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

{$R *.dfm}

uses JcfHelp, JcfSettings, SetComments;

constructor TfComments.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_COMMENTS;
end;

procedure TfComments.Read;
begin
  with FormatSettings.Comments do
  begin
    cbRemoveEmptyDoubleSlashComments.Checked := RemoveEmptyDoubleSlashComments;
    cbRemoveEmptyCurlyBraceComments.Checked := RemoveEmptyCurlyBraceComments;
  end;
end;

procedure TfComments.Write;
begin
  with FormatSettings.Comments do
  begin
    RemoveEmptyDoubleSlashComments := cbRemoveEmptyDoubleSlashComments.Checked;
    RemoveEmptyCurlyBraceComments := cbRemoveEmptyCurlyBraceComments.Checked;
  end;
end;

end.
