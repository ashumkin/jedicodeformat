unit MiscFunctions;

{ AFS 15 Jan 2k

  This project uses very little in the way of internal function libs
  as most is covered by JCL
  I was using CompoentFunctions from my Jedi VCL kit
  however that is causing linkage problems with the IDE plugin - it is a package
  and 2 packages can;t package the same stuff,
  also it creates version dependencies - it bombed with the different version
  of ComponentFunctions that I have at work

  So I am importing just what I need from ComponentFunctions here
}

interface

function PadNumber(const pi: integer): AnsiString;
function StrHasAlpha(const str: AnsiString): boolean;
function GetLastDir(psPath: string): string;

function StrToBoolean(ps: string): boolean;

{ delphi-string wrapper for the win32 pchar api }
function GetWinDir: string;

{not really a file fn - string file name manipulation}
function SetFileNameExtension(const psFileName, psExt: string): string;


implementation

uses
  { delphi } SysUtils, Windows,
  { jcl } JclStrings, JclFileUtils, JclSysUtils;

function StrToBoolean(ps: string): boolean;
begin
  Result := StrIsOneOf(ps, ['t', 'true', 'y', 'yes', '1']);
end;


function PadNumber(const pi: integer): AnsiString;
begin
  Result := IntToStrZeroPad(pi, 3);
end;

function StrHasAlpha(const str: AnsiString): boolean;
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := 1 to Length(str) do
  begin
    if CharIsAlpha(str[liLoop]) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function GetLastDir(psPath: string): string;
var
  liPos: integer;
begin
  Result := '';
  if psPath = '' then
    exit;

  { is this a path ? }
  if not (DirectoryExists(psPath)) and FileExists(psPath) then
  begin
    // must be a file - remove the last bit
    liPos := StrLastPos(PathSeparator, psPath);
    if liPos > 0 then
      psPath := StrLeft(psPath, liPos - 1);
  end;

  liPos := StrLastPos(PathSeparator, psPath);
  if liPos > 0 then
    Result := StrRestOf(psPath, liPos + 1);
end;

function GetWinDir: string;
const
  LEN: integer = 255;
var
  lsBuffer: String;
begin
  SetLength(lsBuffer, LEN);
  FillChar(pChar(lsBuffer)^, LEN, 0);

  GetWindowsDirectory(pChar(lsBuffer), LEN);

  Result := Trim(lsBuffer);
end;

function SetFileNameExtension(const psFileName, psExt: string): string;
var
  liMainFileNameLength: integer;
  lsOldExt: string;
begin
  if PathExtractFileNameNoExt(psFileName) = '' then
  begin
    Result := '';
    exit;
  end;

  lsOldExt  := ExtractFileExt(psFileName);
  liMainFileNameLength := Length(psFileName) - Length(lsOldExt);
  Result := StrLeft(psFileName, liMainFileNameLength);

  Result := Result + '.' + psExt;
end;

end.
