unit JcfNotepadSettings;

{ AFS 2 Jan 2002
  Store Gui state in registry
}

interface

uses
  { delphi } Registry, Classes,
  { local } ConvertTypes;

type
  TJCFNotepadSettings = class(TObject)
  private
    fsInputDir: string;
    fsOutputDir: string;
    fShowParseTreeOption: TShowParseTreeOption;
    fiMRUMaxItems: integer;

    fcReg: TRegIniFile;

    // temp storage
    fbCanClearMRU: Boolean;
    fbClearMRU: Boolean;

    procedure ReadAll;
    procedure WriteAll;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadMRUFiles(const pcFiles: TStrings);
    procedure SaveMRUFiles(const pcFiles: TStrings);

    property InputDir: string read fsInputDir write fsInputDir;
    property OutputDir: string read fsOutputDir write fsOutputDir;
    property MRUMaxItems: integer read fiMRUMaxItems write fiMRUMaxItems;

    property CanClearMRU: Boolean read fbCanClearMRU write fbCanClearMRU;
    property ClearMRU: Boolean read fbClearMRU write fbClearMRU;


    property ShowParseTreeOption: TShowParseTreeOption read fShowParseTreeOption write fShowParseTreeOption;
  end;

const
  REG_SETTINGS_SECTION = 'NotepadSettings';
  REG_FILES_SECTION = 'NotepadFiles';

implementation

uses SysUtils;


constructor TJCFNotepadSettings.Create;
begin
  inherited;

  fcReg := TRegIniFile.Create(REG_ROOT_KEY);

  ReadAll;
end;

destructor TJCFNotepadSettings.Destroy;
begin
  WriteAll;

  FreeAndNil(fcReg);

  inherited;
end;

procedure TJCFNotepadSettings.LoadMRUFiles(const pcFiles: TStrings);
var
  lsKey, lsValue: String;
  liCount, liLoop: integer;
  lcItems: TStringList;
begin
  Assert(pcFiles <> nil);
  pcFiles.Clear;

  liCount := 0;
  lcItems := TStringList.Create;
  try

    while true do
    begin
      lsKey := 'MRUFile' + IntToStr(liCount);
      lsValue := fcReg.ReadString(REG_FILES_SECTION, lsKey, '');
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
      pcFiles.Add(lcItems.Strings[liLoop]);
    end;

  finally
    lcItems.Free;
  end;
end;


procedure TJCFNotepadSettings.SaveMRUFiles(const pcFiles: TStrings);
var
  lsKey: String;
  liLoop: integer;
begin
  Assert(pcFiles <> nil);

  for liLoop := 0 to pcFiles.Count - 1 do
  begin
    lsKey := 'MRUFile' + IntToStr(liLoop);
    fcReg.WriteString(REG_FILES_SECTION, lsKey, pcFiles.Strings[liLoop]);
  end;

  // null-terminate the list
  lsKey := 'MRUFile' + IntToStr(pcFiles.Count);
  fcReg.WriteString(REG_FILES_SECTION, lsKey, '');
end;

procedure TJCFNotepadSettings.ReadAll;
begin
  InputDir := fcReg.ReadString(REG_SETTINGS_SECTION, 'InputDir', '');
  OutputDir := fcReg.ReadString(REG_SETTINGS_SECTION, 'OutputDir', '');
  MRUMaxItems := fcReg.ReadInteger(REG_SETTINGS_SECTION, 'MRUMaxItems', 6);

  ShowParseTreeOption :=  TShowParseTreeOption(
    fcReg.ReadInteger(REG_SETTINGS_SECTION, 'ParseTreeOption', Ord(eShowOnError)));
end;


procedure TJCFNotepadSettings.WriteAll;
begin
  fcReg.WriteString(REG_SETTINGS_SECTION, 'InputDir', InputDir);
  fcReg.WriteString(REG_SETTINGS_SECTION, 'OutputDir', OutputDir);

  fcReg.WriteInteger(REG_SETTINGS_SECTION, 'MRUMaxItems', MRUMaxItems);
  fcReg.WriteInteger(REG_SETTINGS_SECTION, 'ParseTreeOption', Ord(ShowParseTreeOption));
end;

end.

