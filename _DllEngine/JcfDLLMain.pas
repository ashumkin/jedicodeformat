unit JCFDllMain;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfDLLMain.pas, released December 2004.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2004 Anthony Steele.
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

interface

uses ConvertTypes;

{ format actions }
function JcfFormat(const psInput: string): string; stdcall;
procedure JcfProcessFile(const psFileName: string);
procedure JcfFormatFile(const psInputFileName, psOutputFileName: string);

{ setup }
procedure JcfSetOnFormatStatusMessage(const pcProc: TStatusMessageProc); stdcall;
procedure JcfSetOnFileFormatStatusMessage(const pcProc: TStatusMessageProc); stdcall;

procedure JcfClearFormat; stdcall;

procedure JcfReadRegistrySettings; stdcall;
procedure JcfLoadDefaultSettingsFile; stdcall;
procedure JcfLoadSettingsFile(const psFileName: string); stdcall;

{ results of last format }
function JcfConvertError: Boolean; stdcall;
function JcfTokenCount: integer; stdcall;

function JcfFileConvertError: Boolean; stdcall;
function JcfFileTokenCount: integer; stdcall;


{ dialogs }
procedure JcfShowRegistrySettings; stdcall;
procedure JcfShowFormatSettings; stdcall;
procedure JcfShowAbout; stdcall;

procedure JcfLogWrite(const ps: string); stdcall;
procedure JcfCloseLog; stdcall;
procedure JcfCheckShowLog; stdcall;


implementation

uses
  { delphi }
  SysUtils,
  { local }
  Converter, FileConverter, JcfRegistrySettings,
  fRegistrySettings, fAllSettings, fAbout, JCFSettings, JcfLog;

var
  mcConverter: TConverter =  nil;
  mcFileConverter: TFileConverter = nil;

function Converter: TConverter;
begin
  if mcConverter = nil then
  begin
    mcConverter := TConverter.Create;
  end;

  Result := mcConverter;
end;

function FileConverter: TFileConverter;
begin
  if mcFileConverter = nil then
  begin
    mcFileConverter := TFileConverter.Create;
  end;

  Result := mcFileConverter;
end;

procedure JcfSetOnFormatStatusMessage(const pcProc: TStatusMessageProc);
begin
  Converter.OnStatusMessage := pcProc;
end;

procedure JcfSetOnFileFormatStatusMessage(const pcProc: TStatusMessageProc);
begin
  FileConverter.OnStatusMessage := pcProc;
end;


function JcfFormat(const psInput: string): string;
begin
  Converter.InputCode := psInput;
  Converter.Convert;

  if not Converter.ConvertError then
    Result := Converter.OutputCode
  else
    Result := '';
end;

procedure JcfProcessFile(const psFileName: string);
begin
  if not FileExists(psFileName) then
    exit;

  FileConverter.ProcessFile(psFileName);
end;

procedure JcfFormatFile(const psInputFileName, psOutputFileName: string);
begin
  if not FileExists(psInputFileName) then
    exit;

  FileConverter.ConvertFile(psInputFileName, psOutputFileName);
end;


function JcfConvertError: Boolean;
begin
  Result := Converter.ConvertError;
end;

function JcfTokenCount: integer;
begin
  Result := Converter.TokenCount;
end;

function JcfFileConvertError: Boolean;
begin
  Result := FileConverter.ConvertError;
end;

function JcfFileTokenCount: integer;
begin
  Result := Converter.TokenCount;
end;

procedure JcfClearFormat;
begin
  if mcConverter <> nil then
    mcConverter.Clear;
  if mcFileConverter <> nil then
    mcFileConverter.Clear;
end;

procedure JcfReadRegistrySettings;
begin
  GetRegSettings.ReadAll;
end;


procedure JcfShowRegistrySettings;
var
  lcSet: TfmRegistrySettings;
begin
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lcSet := TfmRegistrySettings.Create(nil);
  try
    lcSet.Execute;
  finally
    lcSet.Free;
  end;
end;

procedure JcfShowFormatSettings;
var
  lcSet: TFormAllSettings;
begin
  lcSet := TFormAllSettings.Create(nil);
  try
    lcSet.Execute;
  finally
    lcSet.Free;
  end;

end;

procedure JcfShowAbout;
var
  lcAbout: TfrmAboutBox;
begin
  lcAbout := TfrmAboutBox.Create(nil);
  try
    lcAbout.ShowModal;
  finally
    lcAbout.Free;
  end;
end;

procedure JcfLoadDefaultSettingsFile; stdcall;
begin
  JcfLoadSettingsFile(GetRegSettings.FormatConfigFileName);
end;

procedure JcfLoadSettingsFile(const psFileName: string);
begin
  FormatSettings.ReadFromFile(psFileName);
end;

procedure JcfLogWrite(const ps: string); stdcall;
begin
  Log.Write(ps);
end;

procedure JcfCloseLog;
begin
  Log.CloseLog;
end;

procedure JcfCheckShowLog;
begin
  if GetRegSettings.ViewLogAfterRun then
    GetRegSettings.ViewLog;
end;

initialization
finalization
  FreeAndNil(mcConverter);
end.
