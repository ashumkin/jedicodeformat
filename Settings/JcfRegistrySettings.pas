unit JcfRegistrySettings;

{ AFS 2 Jan 2002
  Store Gui state in registry

  This is not the format options file, that lives in a file so that it can be shared
  This registry file is intended to
   - tell you where the format options file is
   - other GUI config settings that should not be shared, ie
   - logging
}

interface

uses
  { delphi } Registry, Classes,
  { local } ConvertTypes;

type
  TLogLevel = (eLogErrorsOnly, eLogFiles, eLogTokens);
  TLogPlace = (eLogTempDir, eLogAppDIr, eLogSpecifiedDir);


  TJCFRegistrySettings = class(TObject)
  private
    fcReg: TRegIniFile;

    { general settings }
    fShowParseTreeOption: TShowParseTreeOption;

    { ui settings }
    fsFormatConfigFileName: string;
    fsLastSettingsPage: string;

    {notepad settings }
    fsInputDir: string;
    fsOutputDir: string;

    { MRU files settings }
    fiMRUMaxItems: integer;

    { log settings }
    feLogLevel: TLogLevel;
    feLogPlace: TLogPlace;
    fsSpecifiedDirectory: string;
    fbViewLogAfterRun: boolean;
    fbLogTime: boolean;
    fbLogStats: boolean;

    { this is ref not owned }
    fcMRUFiles: TStrings;


    procedure ReadMRUFiles;
    procedure WriteMRUFiles;

  public
    constructor Create;
    destructor Destroy; override;

    function CanClearMRU: Boolean;
    procedure ClearMRU;

    procedure ReadAll;
    procedure WriteAll;

    { general settings }
    property FormatConfigFileName: string read fsFormatConfigFileName write fsFormatConfigFileName;

    { ui settings }
    property ShowParseTreeOption: TShowParseTreeOption read fShowParseTreeOption write fShowParseTreeOption;
    property LastSettingsPage: string read fsLastSettingsPage write fsLastSettingsPage;

    { notepad settings }
    property InputDir: string read fsInputDir write fsInputDir;
    property OutputDir: string read fsOutputDir write fsOutputDir;

    { MRU files settings }
    property MRUMaxItems: integer read fiMRUMaxItems write fiMRUMaxItems;
    property MRUFiles: TStrings read fcMRUFiles write fcMRUFiles;

    { log settings }
    function LogDirectory: string;
    function LogFileName: string;

    property LogLevel: TLogLevel read feLogLevel write feLogLevel;
    property LogPlace: TLogPlace read feLogPlace write feLogPlace;
    property SpecifiedDirectory: string read fsSpecifiedDirectory write fsSpecifiedDirectory;

    property ViewLogAfterRun: boolean read fbViewLogAfterRun write fbViewLogAfterRun;
    property LogTime: boolean read fbLogTime write fbLogTime;
    property LogStats: boolean read fbLogStats write fbLogStats;

    procedure ViewLog;
  end;

function GetRegSettings: TJCFRegistrySettings;

implementation

uses
  { delphi }
  SysUtils, Dialogs,
  { jcl }
  JclFileUtils, JclWin32, JclSysInfo, JclShell,
  { jcf }
  JcfMiscFunctions;

const
  REG_GENERAL_SECTION = 'General';
  REG_NOTEPAD_SECTION = 'NotepadSettings';
  REG_MRU_FILES_SECTION = 'MRUFiles';
  REG_LOG_SECTION = 'Log';
  REG_UI_SECTION = 'UI';

  REG_LOG_LEVEL = 'LogLevel';
  REG_LOG_PLACE = 'LogPlace';
  REG_SPECIFIED_DIRECTORY = 'SpecifiedDirectory';
  REG_VIEW_LOG_AFTER_RUN = 'ViewLogAfterRun';
  REG_LOG_TIME  = 'LogTime';
  REG_LOG_STATS = 'LogStats';

  REG_LAST_SETTINGS_PAGE = 'LastSettingsPage';


{ AFS 10 Oct 2001
 Migrate to file-based settings,  ie
  - read from the settings file if it exists, else use the registry
  - always write to the file
 }
function GetDefaultSettingsFileName: string;
begin
  Result := PathAddSeparator(GetWinDir) + 'JCFSettings.cfg';
end;


constructor TJCFRegistrySettings.Create;
begin
  inherited;
  fcReg := TRegIniFile.Create(REG_ROOT_KEY);
end;

destructor TJCFRegistrySettings.Destroy;
begin
  FreeAndNil(fcReg);
  inherited;
end;

procedure TJCFRegistrySettings.ReadMRUFiles;
var
  lsKey, lsValue: String;
  liCount, liLoop: integer;
  lcItems: TStringList;
begin
  Assert(fcMRUFiles <> nil);
  fcMRUFiles.Clear;

  liCount := 0;
  lcItems := TStringList.Create;
  try

    while true do
    begin
      lsKey := 'MRUFile' + IntToStr(liCount);
      lsValue := fcReg.ReadString(REG_MRU_FILES_SECTION, lsKey, '');
      if lsValue = '' then
        break // done
      else
        lcItems.Add(lsValue);

      inc(liCount);
    end;

    { add them in reverse order to work around a bug in TJvMRUManager
      where the ordre reverses every time it is read/written
    }
    for liLoop := lcItems.Count - 1 downto 0 do
    begin
      fcMRUFiles.Add(lcItems.Strings[liLoop]);
    end;

  finally
    lcItems.Free;
  end;
end;


procedure TJCFRegistrySettings.WriteMRUFiles;
var
  lsKey: String;
  liLoop: integer;
begin
  Assert(fcMRUFiles <> nil);

  for liLoop := 0 to fcMRUFiles.Count - 1 do
  begin
    lsKey := 'MRUFile' + IntToStr(liLoop);
    fcReg.WriteString(REG_MRU_FILES_SECTION, lsKey, fcMRUFiles.Strings[liLoop]);
  end;

  // null-terminate the list
  lsKey := 'MRUFile' + IntToStr(fcMRUFiles.Count);
  fcReg.WriteString(REG_MRU_FILES_SECTION, lsKey, '');
end;

procedure TJCFRegistrySettings.ReadAll;
begin
  { general section }
  fsFormatConfigFileName := fcReg.ReadString(REG_GENERAL_SECTION, 'FormatConfigFileName', '');
  if fsFormatConfigFileName = '' then
    fsFormatConfigFileName := GetDefaultSettingsFileName;


  {notpad settings }
  InputDir := fcReg.ReadString(REG_NOTEPAD_SECTION, 'InputDir', '');
  OutputDir := fcReg.ReadString(REG_NOTEPAD_SECTION, 'OutputDir', '');

  { MRU section }
  MRUMaxItems := fcReg.ReadInteger(REG_NOTEPAD_SECTION, 'MRUMaxItems', 6);
  ReadMRUFiles;

  { log section }
  feLogLevel := TLogLevel(fcReg.ReadInteger(REG_LOG_SECTION, REG_LOG_LEVEL, Ord(eLogFiles)));
  feLogPlace := TLogPlace(fcReg.ReadInteger(REG_LOG_SECTION, REG_LOG_PLACE, Ord(eLogTempDir)));
  fsSpecifiedDirectory := fcReg.ReadString(REG_LOG_SECTION, REG_SPECIFIED_DIRECTORY, 'c:\');
  fbViewLogAfterRun := fcReg.ReadBool(REG_LOG_SECTION, REG_VIEW_LOG_AFTER_RUN, False);
  fbLogTime  := fcReg.ReadBool(REG_LOG_SECTION, REG_LOG_TIME, False);
  fbLogStats := fcReg.ReadBool(REG_LOG_SECTION, REG_LOG_STATS, False);

  { ui }
  fsLastSettingsPage := fcReg.ReadString(REG_UI_SECTION, REG_LAST_SETTINGS_PAGE, '');
  ShowParseTreeOption :=  TShowParseTreeOption(
    fcReg.ReadInteger(REG_UI_SECTION, 'ParseTreeOption', Ord(eShowOnError)));
end;


procedure TJCFRegistrySettings.WriteAll;
begin
  { general section }
  fcReg.WriteString(REG_GENERAL_SECTION, 'FormatConfigFileName', fsFormatConfigFileName);

  { notepad section }
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'InputDir', InputDir);
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'OutputDir', OutputDir);

  { mru section}
  fcReg.WriteInteger(REG_MRU_FILES_SECTION, 'MRUMaxItems', MRUMaxItems);
  WriteMRUFiles;

  { log section }
  fcReg.WriteInteger(REG_LOG_SECTION, REG_LOG_LEVEL, Ord(feLogLevel));
  fcReg.WriteInteger(REG_LOG_SECTION, REG_LOG_PLACE, Ord(feLogPlace));
  fcReg.WriteString(REG_LOG_SECTION, REG_SPECIFIED_DIRECTORY, fsSpecifiedDirectory);
  fcReg.WriteBool(REG_LOG_SECTION, REG_VIEW_LOG_AFTER_RUN, fbViewLogAfterRun);
  fcReg.WriteBool(REG_LOG_SECTION, REG_LOG_TIME, fbLogTime);
  fcReg.WriteBool(REG_LOG_SECTION, REG_LOG_STATS, fbLogStats);

  { ui section }
  fcReg.WriteString(REG_UI_SECTION, REG_LAST_SETTINGS_PAGE, fsLastSettingsPage);
  fcReg.WriteInteger(REG_UI_SECTION, 'ParseTreeOption', Ord(ShowParseTreeOption));
end;

function TJCFRegistrySettings.CanClearMRU: Boolean;
begin
  Result := (MRUFiles <> nil) and (MRUFiles.Count > 0);
end;

procedure TJCFRegistrySettings.ClearMRU;
begin
  if MRUFiles <> nil then
    MRUFiles.Clear;
end;


function TJCFRegistrySettings.LogDirectory: string;
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

function TJCFRegistrySettings.LogFileName: string;
begin
  Result := LogDirectory + 'JediCodeFormat.log';
end;

procedure TJCFRegistrySettings.ViewLog;
var
  lsFile: string;
begin
  lsFile := LogFileName;

  if FileExists(lsFile) then
  begin
    ShellExecEx('notepad.exe ', lsFile);
  end
  else
    ShowMessage('No log file found');
end;


{--------------------------------------------
  singleton accessor }

var
  mcRegistrySettings: TJCFRegistrySettings = nil;

function GetRegSettings: TJCFRegistrySettings;
begin
  if mcRegistrySettings = nil then
    mcRegistrySettings := TJCFRegistrySettings.Create;

  Result := mcRegistrySettings;
end;

initialization
finalization
  FreeAndNil(mcRegistrySettings);
end.

