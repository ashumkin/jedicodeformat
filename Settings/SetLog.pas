{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetLog.pas, released April 2000.
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

unit SetLog;

{ settings to do with logging
  AFS 29 Jan 2K
}

interface

uses
  { delphi } Classes,
  { local } JCFSetBase, TokenType, SettingsStream;

type

  TLogLevel = (eLogErrorsOnly, eLogFiles, eLogTokens);
  TLogPlace = (eLogTempDir, eLogAppDIr, eLogSpecifiedDir);

  TSetLog = class(TSetBase)
  private

    feLogLevel: TLogLevel;
    feLogPlace: TLogPlace;
    fsSpecifiedDirectory: string;
    fbViewLogAfterRun: boolean;
    fbLogTime: boolean;
    fbLogStats: boolean;

  protected

  public
    constructor Create;

    function LogDirectory: string;
    function LogFileName: string;

    procedure ViewLog;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property LogLevel: TLogLevel read feLogLevel write feLogLevel;
    property LogPlace: TLogPlace read feLogPlace write feLogPlace;
    property SpecifiedDirectory: string read fsSpecifiedDirectory write
      fsSpecifiedDirectory;


    property ViewLogAfterRun: boolean read fbViewLogAfterRun write fbViewLogAfterRun;
    property LogTime: boolean read fbLogTime write fbLogTime;
    property LogStats: boolean read fbLogStats write fbLogStats;

  end;

implementation

uses
    { delphi } SysUtils, Windows, Dialogs,
    { jcl } JclStrings, JclSysInfo, JclFileUtils,
    { local } JcfMiscFunctions;

const
  REG_LOG_LEVEL = 'LogLevel';
  REG_LOG_PLACE = 'LogPlace';
  REG_SPECIFIED_DIRECTORY = 'SpecifiedDirectory';
  REG_VIEW_LOG_AFTER_RUN = 'ViewLogAfterRun';
  REG_LOG_TIME  = 'LogTime';
  REG_LOG_STATS = 'LogStats';

  { TSetLog }

constructor TSetLog.Create;
begin
  inherited;
  SetSection('Log');
end;

function TSetLog.LogDirectory: string;
begin
  case feLogPlace of
    eLogTempDir:
      Result := GetWindowsTempFolder;
    eLogAppDir:
      Result := PathAddSeparator(ExtractFileDir(ParamStr(0)));
    eLogSpecifiedDir:
      Result := fsSpecifiedDirectory;
  end;

  Result := PathAddSeparator(Result);
end;

function TSetLog.LogFileName: string;
begin
  Result := LogDirectory + 'CodeFormat.log';
end;

procedure TSetLog.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feLogLevel := TLogLevel(pcStream.Read(REG_LOG_LEVEL, Ord(eLogFiles)));
  feLogPlace := TLogPlace(pcStream.Read(REG_LOG_PLACE, Ord(eLogTempDir)));
  fsSpecifiedDirectory := pcStream.Read(REG_SPECIFIED_DIRECTORY, 'c:\');
  fbViewLogAfterRun := pcStream.Read(REG_VIEW_LOG_AFTER_RUN, False);
  fbLogTime  := pcStream.Read(REG_LOG_TIME, False);
  fbLogStats := pcStream.Read(REG_LOG_STATS, False);
end;


procedure TSetLog.ViewLog;
var
  lsFile: string;
begin
  lsFile := LogFileName;

  if FileExists(lsFile) then
  begin
    WinExec(PChar('notepad.exe ' + lsFile), SW_SHOWDEFAULT);
  end
  else
    ShowMessage('No log file found');
end;

procedure TSetLog.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_LOG_LEVEL, Ord(feLogLevel));
  pcOut.Write(REG_LOG_PLACE, Ord(feLogPlace));
  pcOut.Write(REG_SPECIFIED_DIRECTORY, fsSpecifiedDirectory);
  pcOut.Write(REG_VIEW_LOG_AFTER_RUN, fbViewLogAfterRun);
  pcOut.Write(REG_LOG_TIME, fbLogTime);
  pcOut.Write(REG_LOG_STATS, fbLogStats);
end;


end.