unit JcfNotepadSettings;

{ AFS 2 Jan 2002
  Store Gui state in registry

  This is not the format options file, that lives in a file so that it can be shared
  This registry file is intended to
   - tell you where the format options file is
   - other GUI config settings that should not be shared 
}

interface

uses
  { delphi } Registry, Classes,
  { local } ConvertTypes;

type
  TJCFNotepadSettings = class(TObject)
  private
    fcReg: TRegIniFile;

    { general settings }
    fShowParseTreeOption: TShowParseTreeOption;
    fsFormatConfigFileName: string;

    {notepad settings }
    fsInputDir: string;
    fsOutputDir: string;

    { MRU files settings }
    fiMRUMaxItems: integer;

    { this is ref not owned }
    fcMRUFiles: TStrings;

    procedure ReadAll;
    procedure WriteAll;

    procedure ReadMRUFiles;
    procedure WriteMRUFiles;

  public
    constructor Create;
    destructor Destroy; override;


    { general properties }
    property FormatConfigFileName: string read fsFormatConfigFileName write fsFormatConfigFileName;
    property ShowParseTreeOption: TShowParseTreeOption read fShowParseTreeOption write fShowParseTreeOption;

    { notepad settings }
    property InputDir: string read fsInputDir write fsInputDir;
    property OutputDir: string read fsOutputDir write fsOutputDir;

    { MRU files settings }
    property MRUMaxItems: integer read fiMRUMaxItems write fiMRUMaxItems;
    property MRUFiles: TStrings read fcMRUFiles write fcMRUFiles;
  end;

const
  REG_GENERAL_SECTION = 'General';
  REG_NOTEPAD_SECTION = 'NotepadSettings';
  REG_MRU_FILES_SECTION = 'MRUFiles';

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

procedure TJCFNotepadSettings.ReadMRUFiles;
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


procedure TJCFNotepadSettings.WriteMRUFiles;
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

procedure TJCFNotepadSettings.ReadAll;
begin
  { general section }
  fsFormatConfigFileName := fcReg.ReadString(REG_GENERAL_SECTION, 'FormatConfigFileName', '');
  ShowParseTreeOption :=  TShowParseTreeOption(
    fcReg.ReadInteger(REG_GENERAL_SECTION, 'ParseTreeOption', Ord(eShowOnError)));

  {notpad settings }
  InputDir := fcReg.ReadString(REG_NOTEPAD_SECTION, 'InputDir', '');
  OutputDir := fcReg.ReadString(REG_NOTEPAD_SECTION, 'OutputDir', '');

  { MRU section }
  MRUMaxItems := fcReg.ReadInteger(REG_NOTEPAD_SECTION, 'MRUMaxItems', 6);
  ReadMRUFiles;
end;


procedure TJCFNotepadSettings.WriteAll;
begin
  { general section }
  fcReg.WriteString(REG_GENERAL_SECTION, 'FormatConfigFileName', fsFormatConfigFileName);
  fcReg.WriteInteger(REG_GENERAL_SECTION, 'ParseTreeOption', Ord(ShowParseTreeOption));

  { notepad section }
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'InputDir', InputDir);
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'OutputDir', OutputDir);

  { mru section}
  fcReg.WriteInteger(REG_MRU_FILES_SECTION, 'MRUMaxItems', MRUMaxItems);
  WriteMRUFiles;
end;

end.

