{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Settings.pas, released April 2000.
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

unit JCFSettings;

interface

uses
    { delphi } Classes, Registry,
    { local } SetLog, SetFile,
  SetObfuscate, SetClarify, SetIndent, SetSpaces, SetReturns,
  SetCaps, SetAnyWordCaps,
  SetAlign, SetReplace, SetUses, SetUi,
  SettingsStream, VersionConsts;

type

  TSettings = class(TObject)
  private
    fcLog: TSetLog;
    fcObfuscate: TSetObfuscate;
    fcClarify: TSetClarify;
    fcReturns: TSetReturns;
    fcSpaces: TSetSpaces;
    fcIndent: TSetIndent;

    fcFile: TSetFile;
    fcCaps: TSetCaps;
    fcSpecificWordCaps: TSetAnyWordCaps;
    fcAlign: TSetAlign;
    fcUses: TSetUses;

    fcReplace: TSetReplace;

    fcUi: TSetUi;

    procedure FromStream(const pcStream: TSettingsInput);

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Read;
    procedure Write;

    procedure ToStream(const pcStream: TSettingsOutput);

    procedure ReadFromFile(const psFileName: string);

    property Log: TSetLog read fcLog;
    property FileSettings: TSetFile read FcFile;

    property Obfuscate: TSetObfuscate read fcObfuscate;
    property Clarify: TSetClarify read fcClarify;
    property Indent: TSetIndent read fcIndent;
    property Spaces: TSetSpaces read fcSpaces;
    property Returns: TSetReturns read fcReturns;

    property Caps: TSetCaps read fcCaps;
    property SpecificWordCaps: TSetAnyWordCaps read fcSpecificWordCaps;
    property Align: TSetAlign read fcAlign;
    property Replace: TSetReplace read fcReplace;
    property UsesClause: TSetUses read fcUses;

    // this is not settigns on how to format, it is settings on this program's Ui state
    property UI: TSetUi read fcUi write fcUi;
  end;

function Settings: TSettings;

implementation

uses
  { delphi } SysUtils, Dialogs, Forms,
  { jcl } JclStrings, JclFileUtils,
  { local } ConvertTypes, JCFSetBase, JcfMiscFunctions, RegistrySettings;

var
  // a module var
  mcSettings: TSettings = nil;

function Settings: TSettings;
begin
  if mcSettings = nil then
    mcSettings := TSettings.Create;

  Result := mcSettings;
end;
constructor TSettings.Create;
begin
  inherited;

  fcFile     := TSetFile.Create;
  fcObfuscate     := TSetObfuscate.Create;
  fcClarify     := TSetClarify.Create;
  fcIndent     := TSetIndent.Create;
  fcSpaces     := TSetSpaces.Create;
  fcReturns := TSetReturns.Create;
  fcCaps     := TSetCaps.Create;
  fcSpecificWordCaps     := TSetAnyWordCaps.Create;
  fcAlign     := TSetAlign.Create;
  fcReplace     := TSetReplace.Create;
  fcUses     := TSetUses.Create;
  fcLog     := TSetLog.Create;
  fcUI := TSetUi.Create;

  Read;
end;

destructor TSettings.Destroy;
begin
  Write;

  FreeAndNil(fcLog);
  FreeAndNil(fcFile);
  FreeAndNil(fcObfuscate);
  FreeAndNil(fcClarify);
  FreeAndNil(fcIndent);
  FreeAndNil(fcSpaces);
  FreeAndNil(fcReturns);
  FreeAndNil(fcCaps);
  FreeAndNil(fcSpecificWordCaps);
  FreeAndNil(fcReplace);
  FreeAndNil(fcAlign);
  FreeAndNil(fcUses);
  FreeAndNil(fcUI);

  inherited;
end;

const
  CODEFORMAT_SETTINGS_SECTION = 'JediCodeFormatSettings';
  DEFAULT_SETTINGS_FILE = 'JCFSettings.cfg';


{ AFS 10 Oct 2001
 Migrate to file-based settings,  ie
  - read from the settings file if it exists, else use the registry
  - always write to the file
 }
function SettingsFileName: string;
begin
  Result := PathAddSeparator(GetWinDir) + DEFAULT_SETTINGS_FILE;
end;

procedure TSettings.Read;
var
  lsSettingsFileName: string;
  lsText: string;
  lcFile: TSettingsInputString;
  lcReg: TSettingsInputRegistry;
begin
  // use the Settings File if it exists
  lsSettingsFileName := SettingsFileName;

  if FileExists(lsSettingsFileName) then
  begin
    // debug ShowMessage('Reading settings from file ' + lsSettingsFileName);

      // now we know the file exists - try get settings from it
    lsText := FileToString(lsSettingsFileName);
    lcFile := TSettingsInputString.Create(lsText);
    try
      FromStream(lcFile);
    finally
      lcFile.free;
    end;

  end
  else
  begin
    // debug ShowMessage('Reading settings from registry');

    lcReg := TSettingsInputRegistry.Create(REG_ROOT_KEY);

    try
      FromStream(lcReg);
    finally
      lcReg.free;
    end;

  end;
end;

procedure TSettings.Write;
var
  lcFile: TSettingsStreamOutput;
  lcReg: TSettingsRegistryOutput;
begin
  // use the Settings File if it exists
  lcFile := TSettingsStreamOutput.Create(SettingsFileName);
  try
    ToStream(lcFile);
  finally
    lcFile.free;
  end;

  // debug ShowMessage('Writing registry');
  lcReg := TSettingsRegistryOutput.Create(REG_ROOT_KEY);
  try
    ToStream(lcReg);
  finally
    lcReg.free;
  end;

end;

procedure TSettings.ToStream(const pcStream: TSettingsOutput);

  procedure WriteToStream(const pcSet: TSetBase);
  begin
    Assert(pcSet <> nil);
    pcStream.OpenSection(pcSet.Section);
    pcSet.WriteToStream(pcStream);
    pcStream.CloseSection(pcSet.Section);
  end;

begin
  Assert(pcStream <> nil);
  pcStream.OpenSection(CODEFORMAT_SETTINGS_SECTION);
  pcStream.Write('Version', PROGRAM_VERSION);
  pcStream.Write('WriteDateTime', Now);

  WriteToStream(fcLog);
  WriteToStream(fcFile);
  WriteToStream(fcObfuscate);
  WriteToStream(fcClarify);
  WriteToStream(fcIndent);
  WriteToStream(fcSpaces);
  WritetoStream(fcReturns);
  WriteToStream(fcCaps);
  WriteToStream(fcSpecificWordCaps);
  WriteToStream(fcAlign);
  WriteToStream(fcReplace);
  WriteToStream(fcUses);
  WriteToStream(fcUI);

  pcStream.CloseSection(CODEFORMAT_SETTINGS_SECTION);
end;

procedure TSettings.FromStream(const pcStream: TSettingsInput);
var
  lcAllSettings: TSettingsInput;

  procedure ReadFromStream(const pcSet: TSetBase);
  var
    lcSection: TSettingsInput;
  begin
    Assert(pcSet <> nil);

    lcSection := lcAllSettings.ExtractSection(pcSet.Section);
    if lcSection <> nil then
    begin
      pcSet.ReadFromStream(lcSection);
      lcSection.Free;
    end
    else
      ShowMessage('Skipping section ' + pcSet.Section + ' as it was not found');
  end;

begin

  { basic test - we are only interested in the
    <JediCodeFormatSettings> ... </JediCodeFormatSettings> part of the file
    If this start & end is not present, then is is the wrong file }
  lcAllSettings := pcStream.ExtractSection(CODEFORMAT_SETTINGS_SECTION);
  if lcAllSettings = nil then
  begin
    ShowMessage('No settings found');
    exit;
  end;

  try
    ReadFromStream(fcLog);
    ReadFromStream(fcFile);
    ReadFromStream(fcObfuscate);
    ReadFromStream(fcClarify);
    ReadFromStream(fcIndent);
    ReadFromStream(fcSpaces);
    ReadFromStream(fcReturns);
    ReadFromStream(fcCaps);
    ReadFromStream(fcSpecificWordCaps);
    ReadFromStream(fcAlign);
    ReadFromStream(fcReplace);
    ReadFromStream(fcUses);
    ReadFromStream(fcUI);
  finally
    lcAllSettings.Free;
  end;
end;

procedure TSettings.ReadFromFile(const psFileName: string);
var
  lsText: string;
  lcFile: TSettingsInput;
begin
  Assert(psFileName <> '');
  Assert(FileExists(psFileName));

  // now we know the file exists - try get settings from it
  lsText := FileToString(psFileName);
  lcFile := TSettingsInputString.Create(lsText);
  try
    FromStream(lcFile);
  finally
    lcFile.free;
  end;
end;

end.