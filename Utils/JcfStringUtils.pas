unit JcfSTringUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfSTringUtils, released October 2008.
The Initial Developer of the Original Code is Paul Ishenin 
Portions created by Paul Ishenin are Copyright (C) 1999-2008 Paul Ishenin
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

{
This unit contains string utility code
For use when the JCL string functions are not avaialble
}

interface

uses
  SysUtils, Classes;

const
  AnsiNull           = Char(#0);
  AnsiSoh            = Char(#1);
  AnsiStx            = Char(#2);
  AnsiEtx            = Char(#3);
  AnsiEot            = Char(#4);
  AnsiEnq            = Char(#5);
  AnsiAck            = Char(#6);
  AnsiBell           = Char(#7);
  AnsiBackspace      = Char(#8);
  AnsiTab            = Char(#9);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiVerticalTab    = Char(#11);
  AnsiFormFeed       = Char(#12);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  AnsiSo             = Char(#14);
  AnsiSi             = Char(#15);
  AnsiDle            = Char(#16);
  AnsiDc1            = Char(#17);
  AnsiDc2            = Char(#18);
  AnsiDc3            = Char(#19);
  AnsiDc4            = Char(#20);
  AnsiNak            = Char(#21);
  AnsiSyn            = Char(#22);
  AnsiEtb            = Char(#23);
  AnsiCan            = Char(#24);
  AnsiEm             = Char(#25);
  AnsiEndOfFile      = Char(#26);
  AnsiEscape         = Char(#27);
  AnsiFs             = Char(#28);
  AnsiGs             = Char(#29);
  AnsiRs             = Char(#30);
  AnsiUs             = Char(#31);
  AnsiSpace          = Char(' ');
  AnsiComma          = Char(',');
  AnsiBackslash      = Char('\');
  AnsiForwardSlash   = Char('/');

  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  PathSeparator    = '\';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  AnsiLineBreak = AnsiLineFeed;
  PathSeparator    = '/';
  {$ENDIF UNIX}
  DirDelimiter = PathSeparator;
  AnsiHexDigits      = ['0'..'9', 'A'..'F', 'a'..'f'];
  AnsiWhiteSpace     = [AnsiTab, AnsiLineFeed, AnsiVerticalTab,
    AnsiFormFeed, AnsiCarriageReturn, AnsiSpace];

  AnsiDoubleQuote = Char('"');
  AnsiSingleQuote = Char('''');
const
  // CharType return values
  C1_UPPER  = $0001; // Uppercase
  C1_LOWER  = $0002; // Lowercase
  C1_DIGIT  = $0004; // Decimal digits
  C1_SPACE  = $0008; // Space characters
  C1_PUNCT  = $0010; // Punctuation
  C1_CNTRL  = $0020; // Control characters
  C1_BLANK  = $0040; // Blank characters
  C1_XDIGIT = $0080; // Hexadecimal digits
  C1_ALPHA  = $0100; // Any linguistic character: alphabetic, syllabary, or ideographic

function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;
function CharIsControl(const C: Char): Boolean;
function CharIsDigit(const C: Char): Boolean;
function CharIsReturn(const C: Char): Boolean;
function CharIsWhiteSpace(const C: Char): Boolean;

function CharUpper(const C: Char): Char; 

function StrIsAlpha(const S: string): Boolean;
function StrIsAlphaNum(const S: string): Boolean;

function StrTrimQuotes(const S: string): string;

function StrAfter(const SubStr, S: string): string;
function StrBefore(const SubStr, S: string): string;
function StrChopRight(const S: string; N: Integer): string;
function StrLastPos(const SubStr, S: string): Integer;
function StrLeft(const S: string; Count: Integer): string;
function StrRestOf(const S: string; N: Integer ): string;
function StrRight(const S: string; Count: Integer): string;

function StrDoubleQuote(const S: string): string;
function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;

function StrCharCount(const S: string; C: Char): Integer;
function StrStrCount(const S, SubS: string): Integer;
function StrRepeat(const S: string; Count: Integer): string;
procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;

function BooleanToStr(B: Boolean): string;
function StrToBoolean(const S: string): Boolean;

function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string;
procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );

function FileToString(const FileName: string): AnsiString;
procedure StringToFile(const FileName: string; const Contents: AnsiString);
function StrFillChar(const C: Char; Count: Integer): string;
function IntToStrZeroPad(Value, Count: Integer): AnsiString;
function PathExtractFileNameNoExt(const Path: string): string;
function GetWindowsTempFolder: string;
function FileGetSize(const FileName: string): Int64;

type
  EJcfConversionError = class(Exception)
  end;

implementation
{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

resourcestring
  RsStringToBoolean      = 'Unable to convert the string "%s" to a boolean';

var
  AnsiCharTypes: array [Char] of Word;

procedure LoadCharTypes;
var
  CurrChar: Char;
  CurrType: Word;
  {$IFDEF CLR}
  Category: System.Globalization.UnicodeCategory;
  {$ENDIF CLR}
begin
  for CurrChar := Low(Char) to High(Char) do
  begin
    {$IFDEF MSWINDOWS}
    GetStringTypeExA(LOCALE_USER_DEFAULT, CT_CTYPE1, @CurrChar, SizeOf(Char), CurrType);
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    CurrType := 0;
    if isupper(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_UPPER;
    if islower(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_LOWER;
    if isdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_DIGIT;
    if isspace(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_SPACE;
    if ispunct(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_PUNCT;
    if iscntrl(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_CNTRL;
    if isblank(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_BLANK;
    if isxdigit(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_XDIGIT;
    if isalpha(Byte(CurrChar)) <> 0 then
      CurrType := CurrType or C1_ALPHA;
    {$DEFINE CHAR_TYPES_INITIALIZED}
    {$ENDIF LINUX}
    AnsiCharTypes[CurrChar] := CurrType;
    {$IFNDEF CHAR_TYPES_INITIALIZED}
    Implement case map initialization here
    {$ENDIF ~CHAR_TYPES_INITIALIZED}
  end;
end;

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_ALPHA) <> 0;
end;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := ((AnsiCharTypes[C] and C1_ALPHA) <> 0) or
    ((AnsiCharTypes[C] and C1_DIGIT) <> 0);
end;

function CharIsControl(const C: Char): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_CNTRL) <> 0;
end;

function CharIsDigit(const C: Char): Boolean;
begin
  Result := (AnsiCharTypes[C] and C1_DIGIT) <> 0;
end;

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;

function CharIsWhiteSpace(const C: Char): Boolean;
begin
  Result := C in AnsiWhiteSpace;
end;

function CharUpper(const C: Char): Char;
begin
  // Paul: original code used char case table
  Result := UpCase(C);
end;

function StrIsAlpha(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlpha(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsAlphaNum(const S: string): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not CharIsAlphaNum(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrTrimQuotes(const S: string): string;
var
  First, Last: Char;
  L: Integer;
begin
  L := Length(S);
  if L > 1 then
  begin
    First := S[1];
    Last := S[L];
    if (First = Last) and ((First = AnsiSingleQuote) or (First = AnsiDoubleQuote)) then
      Result := Copy(S, 2, L - 2)
    else
      Result := S;
  end
  else
    Result := S;
end;

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := StrFind(SubStr, S, 1);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;

function StrChopRight(const S: string; N: Integer): string;
begin
  Result := Copy(S, 1, Length(S) - N);
end;

function StrLastPos(const SubStr, S: string): Integer;
var
  Last, Current: PChar;
begin
  Result := 0;
  Last := nil;
  Current := PChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := AnsiStrPos(PChar(Current), PChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs((Longint(PChar(S)) - Longint(Last)) div SizeOf(Char)) + 1;
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrRestOf(const S: string; N: Integer ): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrRight(const S: string; Count: Integer): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

function StrDoubleQuote(const S: string): string;
begin
  Result := AnsiDoubleQuote + S + AnsiDoubleQuote;
end;

function StrSmartCase(const S: string; Delimiters: TSysCharSet): string;
var
  Source, Dest: PChar;
  Index, Len: Integer;
begin
  Result := '';
  if Delimiters = [] then
    Include(Delimiters, AnsiSpace);

  if S <> '' then
  begin
    Result := S;
    UniqueString(Result);

    Len := Length(S);
    Source := PChar(S);
    Dest := PChar(Result);
    Inc(Dest);

    for Index := 2 to Len do
    begin
      if (Source^ in Delimiters) and not (Dest^ in Delimiters) then
        Dest^ := CharUpper(Dest^);
      Inc(Dest);
      Inc(Source);
    end;
    Result[1] := CharUpper(Result[1]);
  end;
end;

function StrCharCount(const S: string; C: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function StrStrCount(const S, SubS: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Length(SubS) > Length(S)) or (Length(SubS) = 0) or (Length(S) = 0) then
    Exit;
  if Length(SubS) = 1 then
  begin
    Result := StrCharCount(S, SubS[1]);
    Exit;
  end;
  I := StrSearch(SubS, S, 1);

  if I > 0 then
    Inc(Result);

  while (I > 0) and (Length(S) > I+Length(SubS)) do
  begin
    I := StrSearch(SubS, S, I+1);

    if I > 0 then
      Inc(Result);
  end
end;

function StrRepeat(const S: string; Count: Integer): string;
var
  Len, Index: Integer;
  Dest, Source: PChar;
begin
  Len := Length(S);
  SetLength(Result, Count * Len);
  Dest := PChar(Result);
  Source := PChar(S);
  if Dest <> nil then
    for Index := 0 to Count - 1 do
  begin
    Move(Source^, Dest^, Len*SizeOf(Char));
    Inc(Dest,Len*SizeOf(Char));
  end;
end;

procedure StrReplace(var S: string; const Search, Replace: string; Flags: TReplaceFlags = []);
begin
  S := StringReplace(S, Search, Replace, Flags);
end;

function StrSearch(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: I expect original code was more efficient :) 
  Result := Pos(SubStr, Copy(S, Index, Length(S)));
end;

function BooleanToStr(B: Boolean): string;
const
  Bools: array [Boolean] of string = ('False', 'True');
begin
  Result := Bools[B];
end;

const
  DefaultTrueBoolStr  = 'True';  // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

  DefaultYesBoolStr   = 'Yes';   // DO NOT LOCALIZE
  DefaultNoBoolStr    = 'No';    // DO NOT LOCALIZE

function StrToBoolean(const S: string): Boolean;
var
  LowerCasedText: string;
begin
  { TODO : Possibility to add localized strings, like in Delphi 7 }
  { TODO : Lower case constants }
  LowerCasedText := LowerCase(S);
  Result := ((S = '1') or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr)) or (LowerCasedText = LowerCase(DefaultYesBoolStr))) or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr[1])) or (LowerCasedText = LowerCase(DefaultYesBoolStr[1]));
  if not Result then
  begin
    Result := not ((S = '0') or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr)) or (LowerCasedText = LowerCase(DefaultNoBoolStr)) or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr[1])) or (LowerCasedText = LowerCase(DefaultNoBoolStr[1])));
    if Result then
      raise EJcfConversionError.CreateResFmt(@RsStringToBoolean, [S]);
  end;
end;


function StrFind(const Substr, S: string; const Index: Integer = 1): Integer;
begin
  // Paul: original code used comparision by char case table
  Result := StrSearch(LowerCase(S), LowerCase(S), Index);
end;

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := StrLeft(S, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean): string;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

procedure TrimStrings(const List: TStrings; DeleteIfEmpty: Boolean = True );
var
  I: Integer;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    for I := List.Count - 1 downto 0 do
    begin
      List[I] := Trim(List[I]);
      if (List[I] = '') and DeleteIfEmpty then
        List.Delete(I);
    end;
  finally
    List.EndUpdate;
  end;
end;

function FileToString(const FileName: string): AnsiString;
var
  fs: TFileStream;
  Len: Integer;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Len := fs.Size;
    SetLength(Result, Len);
    if Len > 0 then
      fs.ReadBuffer(Result[1], Len);
  finally
    fs.Free;
  end;
end;

procedure StringToFile(const FileName: string; const Contents: AnsiString);
var
  fs: TFileStream;
  Len: Integer;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    Len := Length(Contents);
    if Len > 0 then
      fs.WriteBuffer(Contents[1], Len);
  finally
    fs.Free;
  end;
end;

function StrFillChar(const C: Char; Count: Integer): string;
begin
  SetLength(Result, Count);
  if (Count > 0) then
    FillChar(Result[1], Count, Ord(C));
end;

function IntToStrZeroPad(Value, Count: Integer): AnsiString;
begin
  Result := IntToStr(Value);
  if Length(Result) < Count then
    Result := StrFillChar('0', Count - Length(Result)) + Result;
end;

function PathRemoveExtension(const Path: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(':.' + PathSeparator, Path);
  if (I > 0) and (Path[I] = '.') then
    Result := Copy(Path, 1, I - 1)
  else
    Result := Path;
end;

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

function PathRemoveSeparator(const Path: string): string;
var
  L: Integer;
begin
  L := Length(Path);
  if (L <> 0) and (AnsiLastChar(Path) = PathSeparator) then
    Result := Copy(Path, 1, L - 1)
  else
    Result := Path;
end;

function GetWindowsTempFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
    Result := PathRemoveSeparator(Result);
  end;
end;

function FileGetSize(const FileName: string): Int64;
{$IFDEF MSWINDOWS}
var
  SearchRec: TSearchRec;
  OldMode: Cardinal;
  Size: ULARGE_INTEGER;
begin
  Result := -1;
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
    begin
      Size.LowPart := SearchRec.FindData.nFileSizeLow;
      Size.HighPart := SearchRec.FindData.nFileSizeHigh;
      Result := Size.QuadPart;
      SysUtils.FindClose(SearchRec);
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Buf: TStatBuf64;
begin
  Result := -1;
  if GetFileStatus(FileName, Buf, False) = 0 then
    Result := Buf.st_size;
end;
{$ENDIF UNIX}


initialization
  LoadCharTypes;  // this table first

end.

