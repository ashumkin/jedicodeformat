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

{ this is the settings on how to parse. As of 2.0 this is always from a file
  The file name is stored in registry
  This allows centralised settings on a shared dir }

interface

uses
  { delphi } Classes, Registry,
  { local } SetObfuscate, SetClarify,
  SetIndent, SetSpaces, SetReturns,
  SetCaps, SetAnyWordCaps,
  SetAlign, SetReplace, SetUses, 
  SettingsStream, VersionConsts;

type

  TFormatSettings = class(TObject)
  private
    fcObfuscate: TSetObfuscate;
    fcClarify: TSetClarify;
    fcReturns: TSetReturns;
    fcSpaces: TSetSpaces;
    fcIndent: TSetIndent;

    fcCaps: TSetCaps;
    fcSpecificWordCaps: TSetAnyWordCaps;
    fcAlign: TSetAlign;
    fcUses: TSetUses;

    fcReplace: TSetReplace;

    fbWriteOnExit: Boolean;

    procedure FromStream(const pcStream: TSettingsInput);

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Read;
    procedure ReadFromFile(const psFileName: string);
    procedure ReadDefaults;
    procedure Write;

    procedure ToStream(const pcStream: TSettingsOutput);

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

    property WriteOnExit: Boolean read fbWriteOnExit write fbWriteOnExit;
  end;

function FormatSettings: TFormatSettings;

implementation

uses
  { delphi } SysUtils, Dialogs, Forms,
  { jcl } JclStrings,
  { local } ConvertTypes, JCFSetBase, 
   JcfRegistrySettings;


constructor TFormatSettings.Create;
begin
  inherited;

  fcObfuscate := TSetObfuscate.Create;
  fcClarify := TSetClarify.Create;
  fcIndent := TSetIndent.Create;
  fcSpaces := TSetSpaces.Create;
  fcReturns := TSetReturns.Create;
  fcCaps := TSetCaps.Create;
  fcSpecificWordCaps := TSetAnyWordCaps.Create;
  fcAlign := TSetAlign.Create;
  fcReplace := TSetReplace.Create;
  fcUses := TSetUses.Create;

  Read;

  fbWriteOnExit := True;
end;

destructor TFormatSettings.Destroy;
begin
  if WriteOnExit then
    Write;

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
  inherited;
end;

const
  CODEFORMAT_SETTINGS_SECTION = 'JediCodeFormatSettings';

procedure TFormatSettings.Read;
begin
  // use the Settings File if it exists
  ReadFromFile(GetRegSettings.FormatConfigFileName);
end;

procedure TFormatSettings.ReadFromFile(const psFileName: string);
var
  lsText: string;
  lcFile: TSettingsInputString;
begin
  if FileExists(psFileName) then
  begin
    // debug ShowMessage('Reading settings from file ' + lsSettingsFileName);

      // now we know the file exists - try get settings from it
    lsText := FileToString(psFileName);
    lcFile := TSettingsInputString.Create(lsText);
    try
      FromStream(lcFile);
    finally
      lcFile.free;
    end;
  end;
end;


procedure TFormatSettings.ReadDefaults;
var
  lcSetDummy: TSettingsInputDummy;
begin
  lcSetDummy := TSettingsInputDummy.Create;
  try
    FromStream(lcSetDummy);
  finally
    lcSetDummy.free;
  end;
end;

procedure TFormatSettings.Write;
var
  lcFile: TSettingsStreamOutput;
begin
  if GetRegSettings.FormatConfigFileName = '' then
    exit;
    
  // use the Settings File 
  lcFile := TSettingsStreamOutput.Create(GetRegSettings.FormatConfigFileName);
  try
    ToStream(lcFile);
  finally
    lcFile.free;
  end;

end;

procedure TFormatSettings.ToStream(const pcStream: TSettingsOutput);

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

  pcStream.CloseSection(CODEFORMAT_SETTINGS_SECTION);
end;

procedure TFormatSettings.FromStream(const pcStream: TSettingsInput);
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
    <JediCodeFormaTFormatSettings> ... </JediCodeFormaTFormatSettings> part of the file
    If this start & end is not present, then is is the wrong file }
  lcAllSettings := pcStream.ExtractSection(CODEFORMAT_SETTINGS_SECTION);
  if lcAllSettings = nil then
  begin
    ShowMessage('No settings found');
    exit;
  end;

  try
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
  finally
    lcAllSettings.Free;
  end;
end;


var
  // a module var
  mcFormatSettings: TFormatSettings = nil;

function FormatSettings: TFormatSettings;
begin
  if mcFormatSettings = nil then
    mcFormatSettings := TFormatSettings.Create;

  Result := mcFormatSettings;
end;


initialization
finalization
  FreeAndNil(mcFormatSettings);
end.