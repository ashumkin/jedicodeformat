unit frCompilerDirectReturns;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frCompilerDirectReturns.pas, released April 2005.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

{$I JcfGlobal.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  frmBaseSettingsFrame;

type
  TfCompilerDirectReturns = class(TfrSettingsFrame)
    Label1: TLabel;
    rgBeforeUses: TRadioGroup;
    rgBeforeStatements: TRadioGroup;
    rgBeforeGeneral: TRadioGroup;
    rgAfterGeneral: TRadioGroup;
    rgAfterStatements: TRadioGroup;
    rgAfterUses: TRadioGroup;
    Label2: TLabel;
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses JcfSettings, SettingsTypes;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

constructor TfCompilerDirectReturns.Create(AOwner: TComponent);
begin
  inherited;
  //  fiHelpContext := ?? ;
end;

procedure TfCompilerDirectReturns.Read;
begin
  with FormatSettings.Returns do
  begin
    rgBeforeUses.ItemIndex := Ord(BeforeCompilerDirectUses);
    rgBeforeStatements.ItemIndex := Ord(BeforeCompilerDirectStatements);
    rgBeforeGeneral.ItemIndex := Ord(BeforeCompilerDirectGeneral);

    rgAfterUses.ItemIndex := Ord(AfterCompilerDirectUses);
    rgAfterStatements.ItemIndex := Ord(AfterCompilerDirectStatements);
    rgAfterGeneral.ItemIndex := Ord(AfterCompilerDirectGeneral);
  end;
end;

procedure TfCompilerDirectReturns.Write;
begin
  with FormatSettings.Returns do
  begin
    BeforeCompilerDirectUses := TTriOptionStyle(rgBeforeUses.ItemIndex);
    BeforeCompilerDirectStatements := TTriOptionStyle(rgBeforeStatements.ItemIndex);
    BeforeCompilerDirectGeneral := TTriOptionStyle(rgBeforeGeneral.ItemIndex);

    AfterCompilerDirectUses := TTriOptionStyle(rgAfterUses.ItemIndex);
    AfterCompilerDirectStatements := TTriOptionStyle(rgAfterStatements.ItemIndex);
    AfterCompilerDirectGeneral := TTriOptionStyle(rgAfterGeneral.ItemIndex);

  end;
end;

end.
