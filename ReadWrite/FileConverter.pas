unit FileConverter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Converter.pas, released January 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2001 Anthony Steele.
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

interface

uses
  { delphi } Classes,
  { local } Converter, CodeReader, CodeWriter, FileReader, FileWriter,
  ConvertTypes;

{ AFS 11 Jan 2K
  file-specific stuff extracted from converter
  to make way for a editr subclass with the same base class }


type

TFileConverter = class(TConverter)
  private
    fsInput: string;
    peBackupMode: TBackupMode;
    peSourceMode: TSourceMode;

    fsOriginalFileName: string;
    fsOutFileName: string;

    procedure GetFileNames(const psDir: string; psFiles: TStrings);
    procedure GetDirNames(const psDir: string; psFiles: TStrings);

    function FileReader: TFileReader;
    function FileWriter: TFileWriter;

  protected
    function OriginalFileName: string; override;

    function CreateReader: TCodeReader; override;
    function CreateWriter: TCodeWriter; override;

    procedure ProcessDirectory(const psDir: string);

  public
    procedure ProcessFile(const psInput: string);

    procedure Convert; override;

    property BackupMode: TBackupMode read peBackupMode write peBackupMode;
    property SourceMode: TSourceMode read peSourceMode write peSourceMode;
    property Input: string read fsInput write fsInput;

    // details of the last file converted
    property OutFileName: string read fsOutFileName;
  end;

implementation

uses
  { delphi } Windows, SysUtils, Dialogs, Controls, Forms,
  { jcl } JclFileUtils,
  { local } FileUtils, JcfMiscFunctions, JCFLog, JcfRegistrySettings,
  JcfSettings;



procedure TFileConverter.ProcessFile(const psInput: string);
var
  lsMessage, lsOut, lsTemp: string;
  wRes: word;
begin
  if psInput = '' then
  begin
    SendStatusMessage('', 'Select a file', -1, -1);
    exit;
  end;

  if not FileExists(psInput) then
  begin
    SendStatusMessage(psInput, 'The file "' + psInput + '" does not exist', -1, -1);
    exit;
  end;

  if (SourceMode <> fmSingleFile) then
  begin
    lsTemp := PathExtractFileNameNoExt(psInput);

    if GetRegSettings.FileIsExcluded(lsTemp) then
    begin
      Log.Write('Exluded file: ' + psInput);
      exit;
    end;
  end;

  { all kinds of chaos ensues if you work with readonly files,
    for e.g. you can rename them to .bak, but on the next run you will be unable to delete the old .bak files.
    It is only safe when the source is read not written, ie "output to seperate file" backup mode
  }

  if (BackupMode <> cmSeperateOutput) and (FileIsReadOnly(psInput)) then
  begin
    Log.WriteError('File: ' + psInput + ' cannot be processed as it is read only');
    exit;
  end;


  lsMessage := 'Formatting file ' + psInput;

  if GetRegSettings.LogLevel in [eLogFiles, eLogTokens] then
    Log.Write(lsMessage);
  SendStatusMessage(psInput, lsMessage, -1, -1);
  fsOriginalFileName := psInput;

  lsOut := GetRegSettings.GetOutputFileName(psInput, peBackupMode);

  if BackupMode <> cmInplace then
  begin
    if lsOut = '' then
    begin
      SendStatusMessage(psInput, 'No output/backup file specifed', -1, -1);
      exit;
    end;

    if FileExists(lsOut) then
    begin
      if YesAll then
        wRes := mrYes
      else
        wRes := MessageDlg('Output/backup file ' + lsOut + ' exists already. Remove it?',
          mtConfirmation, [mbYes, mbNo, mbAll, mbAbort], 0);

      if wRes = mrAll then
      begin
        YesAll := True;
        wRes     := mrYes;
      end;

      if wRes = mrYes then
      begin
        if not DeleteFile(lsOut) then
          raise Exception.Create('TFileConverter.ProcessFile: ' +
            'Failed to delete file ' + lsOut);
      end
      else if wRes = mrNo then
      begin
        exit;
      end
      else if wRes = mrAbort then
      begin
        fbAbort := True;
        exit;
      end;
    end;
  end;

  if BackupMode = cmInPlace then
  begin
    { rename the original file to an arbitrary temp file,
      write processed code back to the original file,
      delete the temp }


    lsTemp := FileGetTempName('Con');
    if FileExists(lsTemp) then
      if not DeleteFile(lsTemp) then
        raise Exception.Create('TFileConverter.ProcessFile: ' +
          'Failed to delete temp file ' + lsTemp);

    if not RenameFile(psInput, lsTemp) then
      raise Exception.Create('TFileConverter.ProcessFile: ' +
        'could not rename source file ' + psInput + ' to ' + lsTemp);

    FileReader.Clear;
    FileReader.SourceFileName := lsTemp;
    FileWriter.OutputFileName := psInput;
  end
  else if BackupMode = cmInPlaceWithBackup then
  begin
    { rename the original file to the backup file name,
      write processed code back to the original file }


    if not RenameFile(psInput, lsOut) then
      raise Exception.Create('TFileConverter.ProcessFile: ' +
        'could not rename source file ' + psInput + ' to ' + lsOut);

    FileReader.Clear;
    FileReader.SourceFileName := lsOut;
    FileWriter.OutputFileName := psInput;
  end
  else
  begin
    { simple. Source is source, dest is dest }

    FileReader.Clear;
    FileReader.SourceFileName := psInput;
    FileWriter.OutputFileName := lsOut;
    if FileExists(lsOut) then
      raise Exception.Create('TFileConverter.ProcessFile: ' +
        'Destination file ' + lsOut + ' exists already');
  end;

  fsOutFileName := lsOut;
  DoConvertUnit;
  Inc(fiConvertCount);

  // unit is converted. Did it fail?

  if BackupMode = cmInPlaceWithBackup then
  begin
    if ConvertError then
    begin
      // restore the backup
      CopyFile(PChar(lsOut),PChar(psInput), False);
    end

  end
  else if BackupMode = cmInPlace then
  begin
    if ConvertError then
    begin
      // restore the backup
      CopyFile(PChar(lsTemp), PChar(psInput), False);
    end
    else
    begin
      // remove the backup
      if not DeleteFile(lsTemp) then
        Log.WriteError('TFileConverter.ProcessFile: ' +
        'Failed to delete temp file ' + lsTemp + ' for ' + psInput);
    end;

  end;
end;

procedure TFileConverter.ProcessDirectory(const psDir: string);
var
  lsMessage: string;
  lsNames:   TStringList;
  lsDir:     string;
  liLoop:    integer;
begin
  if not DirectoryExists(psDir) then
  begin
    SendStatusMessage('', 'The directory ' + psDir + ' does not exist', -1, -1);
    exit;
  end;

  if GetRegSettings.DirIsExcluded(GetLastDir(psDir)) then
  begin
    Log.Write('Exluded dir: ' + psDir);
    exit;
  end;

  lsDir := IncludeTrailingPathDelimiter(psDir);

  lsMessage := 'Processing directory ' + lsDir;
  //if Settings.Log.LogLevel in [eLogFiles, eLogTokens] then
  Log.Write(lsMessage);
  SendStatusMessage('', lsMessage, -1, -1);

  lsNames := TStringList.Create;
  try { finally free }
    GetFileNames(lsDir, lsNames);

    for liLoop := 0 to lsNames.Count - 1 do
    begin
      ProcessFile(lsDir + lsNames[liLoop]);
      if fbAbort then
        break;

      // refresh the GUI
      Application.ProcessMessages;
    end;

    { all subdirs }
    if SourceMode = fmDirectoryRecursive then
    begin
      lsNames.Clear;
      GetDirNames(lsDir, lsNames);

      for liLoop := 0 to lsNames.Count - 1 do
      begin
        ProcessDirectory(lsDir + lsNames[liLoop]);
        if fbAbort then
          break;
      end;
    end;

  finally
    lsNames.Free;
  end;
end;

procedure TFileConverter.GetFileNames(const psDir: string; psFiles: TStrings);
var
  rSearch: TSearchRec;
  lsName, lsExt, lsSearch: string;
  bDone:   boolean;
begin
  Assert(psDir <> '');
  Assert(psFiles <> nil);

  { for all pas files in the dir }
  lsSearch := psDir + '*.*';
  FillChar(rSearch, Sizeof(TSearchRec), 0);
  bDone := (FindFirst(lsSearch, 0, rSearch) <> 0);

  while not bDone do
  begin
    lsName := rSearch.Name;
    Assert(lsName <> '');
    if (rSearch.Attr and faDirectory > 0) then
      continue;

    lsExt := ExtractFileExt(lsName);
    if (AnsiCompareText(lsExt, '.pas') = 0) or (AnsiCompareText(lsExt, '.dpr') = 0) then
      psFiles.Add(lsName);

    bDone := (FindNext(rSearch) <> 0);
    Assert(bDone or (rSearch.Name <> lsName));
  end;
  FindClose(rSearch);
end;

procedure TFileConverter.GetDirNames(const psDir: string; psFiles: TStrings);
var
  rSearch:  TSearchRec;
  lsSearch: string;
  bDone:    boolean;
begin
  Assert(psDir <> '');
  Assert(psFiles <> nil);

  lsSearch := psDir + '*.*';
  FillChar(rSearch, Sizeof(TSearchRec), 0);
  bDone := (FindFirst(lsSearch, faDirectory, rSearch) <> 0);

  while not bDone do
  begin
    if (rSearch.Attr and faDirectory > 0) and
      (rSearch.Name <> '.') and (rSearch.Name <> '..') then
      psFiles.Add(rSearch.Name);

    bDone := (FindNext(rSearch) <> 0);
  end;
  FindClose(rSearch);
end;


procedure TFileConverter.Convert;
var
  dwStart, dwElapsed: DWord;
begin
  if GetRegSettings.LogTime then
    dwStart := GetTickCount
  else
    dwStart := 0;

  fbAbort  := False;
  fiConvertCount  := 0;

  { all processors must check thier inclusion settings
    as this may have changed from the UI }

  { process file(s) }
  case SourceMode of
    fmSingleFile:
      ProcessFile(Input);
    fmDirectory, fmDirectoryRecursive:
    begin
      ProcessDirectory(Input);
    end
    else
      raise Exception.Create('TConverter.Convert: Bad file recurse type');
  end;

  if GetRegSettings.LogTime then
  begin
    dwElapsed := GetTickCount - dwStart;
    Log.Write('Run took ' + FloatToStr(dwElapsed / 1000) + ' seconds')
  end;

  FinalSummary;
  Log.CloseLog;

  if GetRegSettings.ViewLogAfterRun then
    GetRegSettings.ViewLog;
end;


function TFileConverter.OriginalFileName: string;
begin
  Result := fsOriginalFileName;
end;

function TFileConverter.CreateReader: TCodeReader;
begin
  Result := TFileReader.Create;
end;

function TFileConverter.CreateWriter: TCodeWriter;
begin
  Result := TFileWriter.Create;
end;

function TFileConverter.FileReader: TFileReader;
begin
  Result := fcReader as TFileReader;
end;

function TFileConverter.FileWriter: TFileWriter;
begin
  Result := fcWriter as TFileWriter;
end;

end.