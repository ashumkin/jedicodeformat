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

unit frClarifyLongLineBreaker;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  JvEdit, JvTypedEdit, frmBaseSettingsFrame;

type
  TfClarifyLongLineBreaker = class(TfrSettingsFrame)
    edtMaxLineLength: TJvIntegerEdit;
    Label3: TLabel;
    rgRebreakLongLines: TRadioGroup;
    procedure cbRebreakLinesClick(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses TokenType, JcfSettings, SetReturns, JcfHelp;


constructor TfClarifyLongLineBreaker.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_RETURNS;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyLongLineBreaker.Read;
begin
  with FormatSettings.Returns do
  begin
    { line breaking }
    edtMaxLineLength.Value := MaxLineLength;
    rgRebreakLongLines.ItemIndex := ord(RebreakLines);
  end;
end;

procedure TfClarifyLongLineBreaker.Write;
begin
  with FormatSettings.Returns do
  begin
    { line breaking }
    MaxLineLength := edtMaxLineLength.Value;
    RebreakLines := TWhenToRebreakLines(rgRebreakLongLines.ItemIndex);
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifyLongLineBreaker.cbRebreakLinesClick(Sender: TObject);
begin
  edtMaxLineLength.Enabled := (rgRebreakLongLines.ItemIndex > 0);
end;

end.
