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
  { local }SetObfuscate, SetClarify,
  SetIndent, SetSpaces, SetReturns,
  SetComments, SetCaps, SetWordList,
  SetAlign, SetReplace, SetUses, SetPreProcessor,
  SettingsStream, SetTransform,
  VersionConsts;

type

  TFormatSettings = class(TObject)
  private
    fcObfuscate: TSetObfuscate;
    fcClarify: TSetClarify;
    fcSpaces: TSetSpaces;
    fcIndent: TSetIndent;
    fcReturns: TSetReturns;
    fcComments: TSetComments;

    fcCaps: TSetCaps;
    fcSpecificWordCaps: TSetWordList;
    fcUnitNameCaps: TSetWordList;
    fcPreProcessor: TSetPreProcessor;
    fcAlign: TSetAlign;
    fcUses: TSetUses;

    fcReplace: TSetReplace;

    fcTransform: TSetTransform;

    fbWriteOnExit: boolean;
    fbDirty: boolean;
    fsDescription: string;
    fdtWriteDateTime: TDateTime;
    fsWriteVersion: string;

    procedure FromStream(const pcStream: TSettingsInput);

    procedure DoWrite;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Read;
    procedure ReadFromFile(const psFileName: string);
    procedure ReadDefaults;
    procedure Write;

    procedure MakeConsistent;

    procedure ToStream(const pcStream: TSettingsOutput);

    property Description: string Read fsDescription Write fsDescription;
    property WriteDateTime: TDateTime Read fdtWriteDateTime Write fdtWriteDateTime;
    property WriteVersion: string Read fsWriteVersion Write fsWriteVersion;

    property Obfuscate: TSetObfuscate Read fcObfuscate;
    property Clarify: TSetClarify Read fcClarify;
    property Indent: TSetIndent Read fcIndent;
    property Spaces: TSetSpaces Read fcSpaces;
    property Returns: TSetReturns Read fcReturns;
    property Comments: TSetComments Read fcComments;

    property Caps: TSetCaps Read fcCaps;
    property SpecificWordCaps: TSetWordList Read fcSpecificWordCaps;
    property UnitNameCaps: TSetWordList Read fcUnitNameCaps;
    property PreProcessor: TSetPreProcessor Read fcPreProcessor;


    property Align: TSetAlign Read fcAlign;
    property Replace: TSetReplace Read fcReplace;
    property UsesClause: TSetUses Read fcUses;

    property Transform: TSetTransform read fcTransform;

    property WriteOnExit: boolean Read fbWriteOnExit Write fbWriteOnExit;
    property Dirty: boolean Read fbDirty Write fbDirty;
  end;

function FormatSettings: TFormatSettings;

implementation

uses
  { delphi }SysUtils, Dialogs,
  { jcl }JclStrings,
  { local }JCFSetBase,
  JcfRegistrySettings;


constructor TFormatSettings.Create;
begin
  inherited;

  fcObfuscate := TSetObfuscate.Create;
  fcClarify   := TSetClarify.Create;
  fcIndent    := TSetIndent.Create;
  fcSpaces    := TSetSpaces.Create;
  fcReturns   := TSetReturns.Create;

  fcComments := TSetComments.Create;

  fcCaps := TSetCaps.Create;
  fcSpecificWordCaps := TSetWordList.Create('SpecificWordCaps');
  fcUnitNameCaps := TSetWordList.Create('UnitNameCaps');
  fcPreProcessor := TSetPreProcessor.Create;

  fcAlign   := TSetAlign.Create;
  fcReplace := TSetReplace.Create;
  fcUses    := TSetUses.Create;
  fcTransform := TsetTransform.Create;

  Read;

  fbWriteOnExit := True;
  fbDirty := False;
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
  FreeAndNil(fcComments);

  FreeAndNil(fcCaps);
  FreeAndNil(fcSpecificWordCaps);
  FreeAndNil(fcUnitNameCaps);
  FreeAndNil(fcPreProcessor);

  FreeAndNil(fcReplace);
  FreeAndNil(fcAlign);
  FreeAndNil(fcUses);
  FreeAndNil(fcTransform);
  
  inherited;
end;

const
  CODEFORMAT_SETTINGS_SECTION = 'JediCodeFormatSettings';

  REG_VERSION     = 'WriteVersion';
  REG_WRITE_DATETIME = 'WriteDateTime';
  REG_DESCRIPTION = 'Description';

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
      lcFile.Free;
    end;
  end
  else
  begin
    MessageDlg('The settings file "' + psFileName + '" does not exist.' + AnsiLineBreak +
      'The formatter will work better if it is configured to use a valid settings file',
      mtError, [mbOK], 0);
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
    lcSetDummy.Free;
  end;
end;

procedure TFormatSettings.Write;
var
  lcReg: TJCFRegistrySettings;
begin
  if not Dirty then
    exit;

  { user may have specified no-write }
  lcReg := GetRegSettings;
  if lcReg.FormatFileWriteOption = eNeverWrite then
    exit;

  if lcReg.FormatConfigFileName = '' then
    exit;

  if FileIsReadOnly(lcReg.FormatConfigFileName) then
  begin
    { fail quietly? }
    if lcReg.FormatFileWriteOption = eAlwaysWrite then
        MessageDlg('Error writing settings file: ' +
          lcReg.FormatConfigFileName + ' is read only', mtError, [mbOK], 0);

    exit;
  end;

  try
    DoWrite;
  except
    on e: Exception do
    begin
      if lcReg.FormatFileWriteOption = eAlwaysWrite then
      begin
        MessageDlg('Error writing settings file ' +
          GetRegSettings.FormatConfigFileName + AnsiLineBreak + ' :' +
          E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TFormatSettings.DoWrite;
var
  lcFile: TSettingsStreamOutput;
begin

  // use the Settings File 
  lcFile := TSettingsStreamOutput.Create(GetRegSettings.FormatConfigFileName);
  try
    ToStream(lcFile);

    // not dirty any more 
    fbDirty := False;
  finally
    lcFile.Free;
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
  pcStream.Write(REG_VERSION, PROGRAM_VERSION);
  pcStream.Write(REG_WRITE_DATETIME, Now);
  pcStream.Write(REG_DESCRIPTION, Description);

  WriteToStream(fcObfuscate);
  WriteToStream(fcClarify);
  WriteToStream(fcIndent);
  WriteToStream(fcSpaces);
  WriteToStream(fcReturns);
  WriteToStream(fcComments);
  WriteToStream(fcCaps);
  WriteToStream(fcSpecificWordCaps);
  WriteToStream(fcUnitNameCaps);
  WriteToStream(fcPreProcessor);
  WriteToStream(fcAlign);
  WriteToStream(fcReplace);
  WriteToStream(fcUses);
  WriteToStream(fcTransform);

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
    fsWriteVersion   := pcStream.Read(REG_VERSION, '');
    fsDescription    := pcStream.Read(REG_DESCRIPTION, '');
    fdtWriteDateTime := pcStream.Read(REG_WRITE_DATETIME, 0.0);

    ReadFromStream(fcObfuscate);
    ReadFromStream(fcClarify);
    ReadFromStream(fcIndent);
    ReadFromStream(fcSpaces);
    ReadFromStream(fcReturns);
    ReadFromStream(fcComments);
    ReadFromStream(fcCaps);
    ReadFromStream(fcSpecificWordCaps);
    ReadFromStream(fcUnitNameCaps);
    ReadFromStream(fcPreProcessor);

    ReadFromStream(fcAlign);
    ReadFromStream(fcReplace);
    ReadFromStream(fcUses);
    ReadFromStream(fcTransform);
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


procedure TFormatSettings.MakeConsistent;
begin
  { one consistency check so far
    - if linebreaking is off, then "remove returns in expressions" must also be off }

  if Returns.RebreakLines = rbOff then
    Returns.RemoveExpressionReturns := False;
end;

initialization

finalization
  FreeAndNil(mcFormatSettings);
end.
