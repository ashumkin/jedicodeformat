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

unit frClarifyBlocks;

interface

uses
  { delphi }
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  { local}
  frmBaseSettingsFrame;

type
  TfClarifyBlocks = class(TfrSettingsFrame)
    rgBlockBegin: TRadioGroup;
    rgLabelBegin: TRadioGroup;
    rgLabel: TRadioGroup;
    rgBlock: TRadioGroup;
    rgEndElse: TRadioGroup;
    Label1: TLabel;
    rgCaseLabel: TRadioGroup;
    rgElseIf: TRadioGroup;
    rgElseCase: TRadioGroup;
  private

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$R *.DFM}

uses JcfSettings, SettingsTypes, JcfHelp;

constructor TfClarifyBlocks.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_BLOCKS;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyBlocks.Read;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    rgBlockBegin.ItemIndex := Ord(BlockBeginStyle);
    rgBlock.ItemIndex      := Ord(BlockStyle);
    rgLabelBegin.ItemIndex := Ord(LabelBeginStyle);
    rglabel.ItemIndex      := Ord(LabelStyle);
    rgEndElse.ItemIndex    := Ord(EndElseStyle);
    rgCaseLabel.ItemIndex := Ord(CaseLabelStyle);
    rgElseIf.ItemIndex := Ord(ElseIfStyle);

    rgCaseLabel.ItemIndex := Ord(CaseLabelStyle);
    rgElseCase.ItemIndex := Ord(CaseElseStyle);
  end;
end;

procedure TfClarifyBlocks.Write;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    BlockBeginStyle := TBlockNewLineStyle(rgBlockBegin.ItemIndex);
    BlockStyle      := TBlockNewLineStyle(rgBlock.ItemIndex);
    LabelBeginStyle := TBlockNewLineStyle(rgLabelBegin.ItemIndex);
    LabelStyle      := TBlockNewLineStyle(rgLabel.ItemIndex);
    EndElseStyle    := TBlockNewLineStyle(rgEndElse.ItemIndex);
    ElseIfStyle     := TBlockNewLineStyle(rgElseIf.ItemIndex);

    CaseLabelStyle  := TBlockNewLineStyle(rgCaseLabel.ItemIndex);
    CaseElseStyle   := TBlockNewLineStyle(rgElseCase.ItemIndex);
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

end.