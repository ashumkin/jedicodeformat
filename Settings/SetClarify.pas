{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SetClarify.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2005 Anthony Steele.
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

{ generic clarify options that don't go elsewhere
  Warnings control }

unit SetClarify;

interface

uses
  Classes,
  JCFSetBase, SettingsStream;

type
  // once-offs can be run, not run, or can be the only thing run
  TOnceOffsOption = (eDoNotRun, eDoRun, eRunOnly);

  TSetClarify = class(TSetBase)
  private
    feOnceOffs: TOnceOffsOption;
    fbWarnings: boolean;
    fbWarnUnusedParams: boolean;
    fcIgnoreUnusedParams: TStringList;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property OnceOffs: TOnceOffsOption Read feOnceOffs Write feOnceOffs;

    property Warnings: boolean Read fbWarnings Write fbWarnings;
    property WarnUnusedParams: boolean Read fbWarnUnusedParams Write fbWarnUnusedParams;
    property IgnoreUnusedParams: TStringList read fcIgnoreUnusedParams;
  end;

implementation

uses
  SysUtils;

const
  REG_ONCE_OFFS = 'OnceOffs';
  REG_WARNINGS  = 'Warnings';
  REG_WARN_UNUSED_PARAMS  = 'WarnUnusedParams';
  REG_IGNORE_UNUSED_PARAMS = 'IgnoreUnusedParams';

  { TSetClarify }

constructor TSetClarify.Create;
begin
  inherited;
  SetSection('Clarify');

  fcIgnoreUnusedParams := TStringList.Create;
  fcIgnoreUnusedParams.Duplicates := dupIgnore;
end;

destructor TSetClarify.Destroy;
begin
  FreeAndNil(fcIgnoreUnusedParams);
  inherited;
end;


procedure TSetClarify.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feOnceOffs := TOnceOffsOption(pcStream.Read(REG_ONCE_OFFS, Ord(feOnceOffs)));
  fbWarnings := pcStream.Read(REG_WARNINGS, True);

  fbWarnUnusedParams := pcStream.Read(REG_WARN_UNUSED_PARAMS, True);

  pcStream.Read(REG_IGNORE_UNUSED_PARAMS, fcIgnoreUnusedParams);
  fcIgnoreUnusedParams.Sort;

  // default value
  if (fcIgnoreUnusedParams.Count = 0) and fbWarnUnusedParams then
    fcIgnoreUnusedParams.Add('Sender');
end;

procedure TSetClarify.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ONCE_OFFS, Ord(feOnceOffs));
  
  pcOut.Write(REG_WARNINGS, fbWarnings);
  pcOut.Write(REG_WARN_UNUSED_PARAMS, fbWarnUnusedParams);

  fcIgnoreUnusedParams.Sort;
  pcOut.Write(REG_IGNORE_UNUSED_PARAMS, fcIgnoreUnusedParams);
end;

end.
