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
  { local } Converter, 
  ConvertTypes;

{ AFS 7 July 04
  rewrote this as a wrapper for the string->string converter
  So basically it deals with file issues
  and delegates the convertion to the wrapped TConverter
}


type

  TFileConverter = class(TObject)
  private
    { the string-> string converter }
    fcConverter: TConverter;

    { state }
    fOnStatusMessage: TStatusMessageProc;
    peBackupMode: TBackupMode;
    peSourceMode: TSourceMode;

    { properties }
    fsInput: string;
    fsOriginalFileName: string;
    fsOutFileName: string;
    fbYesAll: boolean;
    fbGuiMessages: Boolean;
    fbAbort: boolean;
    fiConvertCount: integer;

    procedure SendStatusMessage(const psUnit, psMessage: string;
      const piY, piX: integer);

    procedure GetFileNames(const psDir: string; psFiles: TStrings);
    procedure GetDirNames(const psDir: string; psFiles: TStrings);

    function GetOnStatusMessage: TStatusMessageProc;
    procedure SetOnStatusMessage(const Value: TStatusMessageProc);
    procedure FinalSummary;

  protected
    function OriginalFileName: string;

    procedure ProcessDirectory(const psDir: string);

  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessFile(const psInputFileName: string);

    procedure Convert;
    procedure Clear;

    function ConvertError: Boolean;
    function TokenCount: integer;

    procedure ConvertFile(const psInputFileName, psOutputFileName: string);


    property BackupMode: TBackupMode Read peBackupMode Write peBackupMode;
    property SourceMode: TSourceMode Read peSourceMode Write peSourceMode;
    property Input: string Read fsInput Write fsInput;

    property YesAll: boolean read fbYesAll write fbYesAll;
    property GuiMessages: Boolean read fbGuiMessages write fbGuiMessages;

    property Abort: boolean read fbAbort write fbAbort;

    // details of the last file converted
    property OutFileName: string Read fsOutFileName;

    property OnStatusMessage: TStatusMessageProc read GetOnStatusMessage write SetOnStatusMessage;
  end;

implementation

uses
  { delphi }Windows, SysUtils, Dialogs, Controls, Forms,
  { jcl }JclFileUtils, JclStrings,
  { local }FileUtils, JcfMiscFunctions, JCFLog, JcfRegistrySettings;

constructor TFileConverter.Create;
begin
  inherited;
  fcConverter := TConverter.Create;
  fcConverter.OnStatusMessage := SendStatusMessage;
end;

destructor TFileConverter.Destroy;
begin
  FreeAndNil(fcConverter);
  inherited;
end;

procedure TFileConverter.ProcessFile(const psInputFileName: string);
var
  lsMessage, lsOut, lsTemp: string;
  wRes: word;
begin
  if psInputFileName = '' then
  begin
    SendStatusMessage('', 'Select a file', -1, -1);
    exit;
  end;

  if not FileExists(psInputFileName) then
  begin
    SendStatusMessage(psInputFileName, 'The file "' + psInputFileName + '" does not exist', -1, -1);
    exit;
  end;

  if FileGetSize(psInputFileName) < 1 then
  begin
    SendStatusMessage(psInputFileName, 'The file "' + psInputFileName + '" is empty', -1, -1);
    exit;
  end;

  if (SourceMode <> fmSingleFile) then
  begin
    lsTemp := PathExtractFileNameNoExt(psInputFileName);

    if GetRegSettings.FileIsExcluded(lsTemp) then
    begin
      Log.Write('Exluded file: ' + psInputFileName);
      exit;
    end;
  end;

  { all kinds of chaos ensues if you work with readonly files,
    for e.g. you can rename them to .bak, but on the next run you will be unable to delete the old .bak files.
    It is only safe when the source is read not written, ie "output to seperate file" backup mode
  }

  if (BackupMode <> cmSeperateOutput) and (FileIsReadOnly(psInputFileName)) then
  begin
    Log.WriteError('File: ' + psInputFileName + ' cannot be processed as it is read only');
    exit;
  end;

  lsMessage := 'Formatting file ' + psInputFileName;

  if GetRegSettings.LogLevel in [eLogFiles, eLogTokens] then
    Log.Write(lsMessage);
  SendStatusMessage(psInputFileName, lsMessage, -1, -1);
  fsOriginalFileName := psInputFileName;

  lsOut := GetRegSettings.GetOutputFileName(psInputFileName, peBackupMode);

  if BackupMode <> cmInplace then
  begin
    if lsOut = '' then
    begin
      SendStatusMessage(psInputFileName, 'No output/backup file specifed', -1, -1);
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
        wRes   := mrYes;
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

  case BackupMode of
    cmInPlace:
    begin
      fsOutFileName := psInputFileName;

      { rename the original file to an arbitrary temp file,
        write processed code back to the original file,
        delete the temp }
      lsTemp := FileGetTempName('Con');
      if FileExists(lsTemp) then
        if not DeleteFile(lsTemp) then
          raise Exception.Create('TFileConverter.ProcessFile: ' +
            'Failed to delete temp file ' + lsTemp);

      if not RenameFile(psInputFileName, lsTemp) then
        raise Exception.Create('TFileConverter.ProcessFile: ' +
          'could not rename source file ' + psInputFileName + ' to ' + lsTemp);

      { process from the temp file to the input file }
      ConvertFile(lsTemp, psInputFileName);

      if ConvertError then
      begin
        // restore the backup
        CopyFile(pchar(lsTemp), pchar(psInputFileName), False);
      end
      else
      begin
        Inc(fiConvertCount);

        // remove the backup
        if not DeleteFile(lsTemp) then
          Log.WriteError('TFileConverter.ProcessFile: ' +
            'Failed to delete temp file ' + lsTemp + ' for ' + psInputFileName);
      end;

    end;

    cmInPlaceWithBackup:
    begin
      fsOutFileName := psInputFileName;

      { rename the original file to the backup file name,
        write processed code back to the original file }
      if not RenameFile(psInputFileName, lsOut) then
        raise Exception.Create('TFileConverter.ProcessFile: ' +
          'could not rename source file ' + psInputFileName + ' to ' + lsOut);

      { process from the temp file to the input file }
      ConvertFile(lsOut, psInputFileName);

      // did the convert fail?
      if ConvertError then
      begin
        // oops! restore the backup
        CopyFile(pchar(lsOut), pchar(psInputFileName), False);
      end
      else
      begin
        Inc(fiConvertCount);
      end;
    end;

    cmSeperateOutput:
    begin
      { simple. Source is source, dest is dest }
      fsOutFileName := lsOut;

      if FileExists(lsOut) then
        raise Exception.Create('TFileConverter.ProcessFile: ' +
          'Destination file ' + lsOut + ' exists already');

      ConvertFile(psInputFileName, lsOut);

      if not ConvertError then
        Inc(fiConvertCount);
    end;

    else
      Assert(False, 'Bad backup mode');
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

  fbAbort := False;
  fiConvertCount := 0;

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

procedure TFileConverter.ConvertFile(const psInputFileName, psOutputFileName: string);
begin
  fcConverter.InputCode := FileToString(psInputFileName);
  fcConverter.Convert;
  if not ConvertError then
    StringToFile(psOutputFileName, fcConverter.OutputCode);
end;


function TFileConverter.OriginalFileName: string;
begin
  Result := fsOriginalFileName;
end;


procedure TFileConverter.FinalSummary;
var
  lsMessage: string;
begin
  if fiConvertCount = 0 then
  begin
    if ConvertError then
      lsMessage := 'Aborted due to error'
    else
      lsMessage := 'Nothing done';
  end
  else if fbAbort then
    lsMessage := 'Aborted after ' + DescribeFileCount(fiConvertCount)
  else if fiConvertCount > 1 then
    lsMessage := 'Finished processing ' + DescribeFileCount(fiConvertCount)
  else
    lsMessage := '';

  if lsMessage <> '' then
  begin
    SendStatusMessage('', lsMessage, -1, -1);

    Log.EmptyLine;
    Log.Write(lsMessage);
  end;
end;

procedure TFileConverter.Clear;
begin
  fcConverter.Clear;
end;


function TFileConverter.ConvertError: Boolean;
begin
  Result := fcConverter.ConvertError;
end;


function TFileConverter.TokenCount: integer;
begin
  Result := fcConverter.TokenCount;
end;

function TFileConverter.GetOnStatusMessage: TStatusMessageProc;
begin
  Result := fOnStatusMessage;
end;

procedure TFileConverter.SetOnStatusMessage(const Value: TStatusMessageProc);
begin
  fOnStatusMessage := Value;
end;

procedure TFileConverter.SendStatusMessage(const psUnit, psMessage: string;
  const piY, piX: integer);
var
  lsUnit: string;
begin
  lsUnit := psUnit;
  if lsUnit = '' then
    lsUnit := OriginalFileName;

  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(lsUnit, psMessage, piY, piX);
end;

end.
