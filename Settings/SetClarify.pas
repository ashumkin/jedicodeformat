{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SetClarify.pas, released April 2000.
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

{ generic clarify options that don't go elsewhere }

unit SetClarify;

interface

uses JCFSetBase, TokenType, SettingsStream;

type
  // once-offs can be run, not run, or can be the only thing run
  TOnceOffsOption = (eDoNotRun, eDoRun, eRunOnly);

  TSetClarify = class(TSetBase)
  private
    feOnceOffs: TOnceOffsOption;
    fbWarnings: boolean;

  protected

  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property OnceOffs: TOnceOffsOption read feOnceOffs write feOnceOffs;
    property Warnings: boolean read fbWarnings write fbWarnings;
  end;

implementation

const
  REG_ONCE_OFFS = 'OnceOffs';
  REG_WARNINGS  = 'Warnings';

  { TSetClarify }

constructor TSetClarify.Create;
begin
  inherited;
  SetSection('Clarify');
end;

procedure TSetClarify.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feOnceOffs := TOnceOffsOption(pcStream.Read(REG_ONCE_OFFS, Ord(feOnceOffs)));
  fbWarnings := pcStream.Read(REG_WARNINGS, True);
end;

procedure TSetClarify.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ONCE_OFFS, Ord(feOnceOffs));
  pcOut.Write(REG_WARNINGS, fbWarnings);
end;

end.