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

unit frClarifySpaces;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  JvTypedEdit, JvEdit, frmBaseSettingsFrame;

type
  TfClarifySpaces = class(TfrSettingsFrame)
    cbFixSpacing: TCheckBox;
    cbSpaceClassHeritage: TCheckBox;
    gbColon: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    eSpaceBeforeColonVar: TJvIntegerEdit;
    eSpaceBeforeColonParam: TJvIntegerEdit;
    eSpaceBeforeColonFn: TJvIntegerEdit;
    eSpacesBeforeColonClassVar: TJvIntegerEdit;
    gbTabs: TGroupBox;
    cbTabsToSpaces: TCheckBox;
    cbSpacesToTabs: TCheckBox;
    Label1: TLabel;
    edtSpacesPerTab: TJvIntegerEdit;
    Label3: TLabel;
    edtSpacesForTab: TJvIntegerEdit;
    eSpacesBeforeCaseLabel: TJvIntegerEdit;
    eSpacesBeforeLabel: TJvIntegerEdit;
    Label5: TLabel;
    Label6: TLabel;
    procedure cbTabsToSpacesClick(Sender: TObject);
    procedure cbSpacesToTabsClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses JcfHelp, JcfSettings;

constructor TfClarifySpaces.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_SPACES;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifySpaces.Read;
begin
  with FormatSettings.Spaces do
  begin
    cbTabsToSpaces.Checked := TabsToSpaces;
    cbSpacesToTabs.Checked := SpacesToTabs;
    edtSpacesPerTab.Value  := SpacesPerTab;
    edtSpacesForTab.Value  := SpacesForTab;

    cbFixSpacing.Checked   := FixSpacing;

    cbSpaceClassHeritage.Checked := SpaceBeforeClassHeritage;

    eSpaceBeforeColonVar.Value := SpacesBeforeColonVar;
    eSpaceBeforeColonParam.Value := SpacesBeforeColonParam;
    eSpaceBeforeColonFn.Value := SpacesBeforeColonFn;
    eSpacesBeforeColonClassVar.Value := SpacesBeforeColonClassVar;

    eSpacesBeforeCaseLabel.Value := SpacesBeforeColonCaseLabel;
    eSpacesBeforeLabel.Value := SpacesBeforeColonLabel;
  end;

  cbTabsToSpacesClick(nil);
  cbSpacesToTabsClick(nil);
end;

procedure TfClarifySpaces.Write;
begin
  with FormatSettings.Spaces do
  begin
    TabsToSpaces := cbTabsToSpaces.Checked;
    SpacesToTabs := cbSpacesToTabs.Checked;

    SpacesPerTab := edtSpacesPerTab.Value;
    SpacesForTab := edtSpacesForTab.Value;

    FixSpacing   := cbFixSpacing.Checked;

    SpaceBeforeClassHeritage := cbSpaceClassHeritage.Checked;

    SpacesBeforeColonVar := eSpaceBeforeColonVar.Value;
    SpacesBeforeColonParam := eSpaceBeforeColonParam.Value;
    SpacesBeforeColonFn := eSpaceBeforeColonFn.Value;
    SpacesBeforeColonClassVar := eSpacesBeforeColonClassVar.Value;
    SpacesBeforeColonCaseLabel := eSpacesBeforeCaseLabel.Value;
    SpacesBeforeColonLabel := eSpacesBeforeLabel.Value;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifySpaces.cbTabsToSpacesClick(Sender: TObject);
begin
  edtSpacesPerTab.Enabled := cbTabsToSpaces.Checked;
end;

procedure TfClarifySpaces.cbSpacesToTabsClick(Sender: TObject);
begin
  edtSpacesForTab.Enabled := cbSpacesToTabs.Checked;
end;

end.