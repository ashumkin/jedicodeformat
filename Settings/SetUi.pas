unit SetUi;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetUi.pas, released June 2001.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2001 Anthony Steele.
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

{ settings not tot do with code formatting, but to do with this program's state
  AFS 17 June 2001 }

uses JCFSetBase, TokenType, SettingsStream;

type

  TSetUi = class(TSetBase)
  private

    fsLastSettingsPage: string;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property LastSettingsPage: string read fsLastSettingsPage write fsLastSettingsPage;

  end;

implementation

const
  REG_LAST_SETTINGS_PAGE = 'LastSettingsPage';


constructor TSetUi.Create;
begin
  inherited;
  SetSection('UI');
end;

procedure TSetUi.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fsLastSettingsPage := pcStream.Read(REG_LAST_SETTINGS_PAGE, '');
end;

procedure TSetUi.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);
  pcOut.Write(REG_LAST_SETTINGS_PAGE, fsLastSettingsPage);
end;


end.
