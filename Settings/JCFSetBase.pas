{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetBase.pas, released April 2000.
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

unit JCFSetBase;

{ base class for a group of settings
  AFS 29 Dec 1999

}

interface

uses
    { delphi } Classes,
    { local } SettingsStream;

type

  TSetBase = class(TObject)
  private
    fsSection: string;

  protected
    procedure SetSection(const ps: string);

  public
    procedure WriteToStream(const pcStream: TSettingsOutput); virtual;
    procedure ReadFromStream(const pcStream: TSettingsInput); virtual;

    property Section: string read fsSection;
  end;

implementation

uses
 { delphi } SysUtils,
 { jcl } JclStrings,
 { local } JcfMiscFunctions;


procedure TSetBase.SetSection(const ps: string);
begin
  fsSection := ps;
end;

procedure TSetBase.WriteToStream(const pcStream: TSettingsOutput);
begin
 // do nothing - here for override
 Assert(False, 'Class ' + ClassName + ' must override TSetBase.WriteToStream');
end;

procedure TSetBase.ReadFromStream(const pcStream: TSettingsInput);
begin
 // do nothing - here for override
 Assert(False, 'Class ' + ClassName + ' must override TSetBase.ReadFromStream');
end;



end.