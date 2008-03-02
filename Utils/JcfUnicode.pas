unit JcfUnicode;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfUnicode, released March 2007.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
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

type
  TFileContentType = (eUnknown, e8Bit, eUtf8,
    eUtf16LittleEndian, eUtf16BigEndian);

procedure ReadTextFile(const psFileName: string; out psContents: WideString;
  out peContentType: TFileContentType);

procedure WriteTextFile(const psFileName: string; const psContents: WideString;
  const peContentType: TFileContentType);

implementation

uses
  Classes, SysUtils;


{ read in a text file,
  the file can contain 8-bit or 16-bit chars
  code adapted from a sample by Mike Shkolnik
  in nntp://borland.public.delphi.rtl.general
  Re: Read UNICODE/ANSI/ASCII Text File to WideString
  at: Jan 23 2006, 12:17
  found at http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.rtl.general&messageid=43d485bf$1@newsgroups.borland.com
}
procedure ReadTextFile(const psFileName: string; out psContents: WideString;
  out peContentType: TFileContentType);
var
  fs: TFileStream;
  wordRead: word;
  byteRead: byte;
  wideContents: WideString;
  contents8bit: string;
  liBytesRemaining: integer;
  liLoop: integer;
  lbResetPosition: boolean;
const
  // marker bytes at the start of the file

  /// 3 bytes for UTF-8
  Utf8Marker12 = $BBEF;
  Utf8Marker3  = $BF;

  // 4 bytes for UTF-16. Big or little-endian
  Utf16LittleEndianMarker = $FEFF;
  Utf16BigEndianMarker = $FFFE;

  // 4 bytes for utf-32. Big or little-endian
  Utf32LittleEndianMarker1 = $FEFF;
  Utf32LittleEndianMarker2 = $0000;

  Utf32BigEndianMarker1 = $0000;
  Utf32BigEndianMarker2 = $FFFE;

begin
  psContents    := '';
  peContentType := eUnknown;

  {open file}
  fs := TFileStream.Create(psFileName, fmOpenRead);
  try

    { the stream can contain unicode characters -
       we must check before parse }
    fs.Read(wordRead, SizeOf(wordRead));
    if ((wordRead = Utf16LittleEndianMarker) or (wordRead = Utf16BigEndianMarker)) then
    begin
      if (fs.Size > fs.Position) then
      begin
        // read it
        liBytesRemaining := fs.Size - fs.Position;
        SetLength(wideContents, liBytesRemaining div 2);
        fs.Read(wideContents[1], liBytesRemaining);

        if (wordRead = Utf16BigEndianMarker) then
        begin
          peContentType := eUtf16BigEndian;

          // swap the bytes
          for liLoop := 1 to Length(wideContents) do
            wideContents[liLoop] := widechar(Swap(word(wideContents[liLoop])));
        end
        else
        begin
          peContentType := eUtf16LittleEndian;
        end;

        psContents := wideContents;
      end;
    end
    else
    begin
      lbResetPosition := True;

      // the file is 8-bit, but check for UTF-8 marker bytes
      // which unlike the other 2-byte codes
      // is 3 bytes long
      if wordRead = Utf8Marker12 then
      begin
        fs.Read(byteRead, SizeOf(byteRead));
        if byteRead = Utf8Marker3 then
        begin
          peContentType   := eUtf8;
          lbResetPosition := False;
        end;
      end;

      if peContentType = eUnknown then
      begin
        // it's just an 8-bit text file
        peContentType := e8Bit;
      end;

      if lbResetPosition then
        fs.Seek(0, soFromBeginning);

      liBytesRemaining := fs.Size - fs.Position;
      // read the bytes into a string
      SetLength(contents8bit, liBytesRemaining);
      if fs.Size > 0 then
      begin
        fs.ReadBuffer(contents8bit[1], liBytesRemaining);
      end;

      // convert to wide char 
      psContents := contents8bit;
    end;

  finally
    // close the file
    fs.Free;
  end;
end;

procedure WriteTextFile(const psFileName: string; const psContents: WideString;
  const peContentType: TFileContentType);
var
  fs:     TFileStream;
  Len:    integer;
  lsContents: string;
  liLoop: integer;
  wChar:  word;
begin
  fs := TFileStream.Create(psFileName, fmCreate);
  try

    if peContentType = e8Bit then
    begin
      lsContents := psContents;
      Len := Length(lsContents);
      if Len > 0 then
      begin
        fs.WriteBuffer(lsContents[1], Len);
      end;

    end
    else if peContentType = eUtf16LittleEndian then
    begin
      Len := Length(psContents);
      if Len > 0 then
      begin
        fs.WriteBuffer(psContents[1], Len * 2);
      end;

    end
    else if peContentType = eUtf16BigEndian then
    begin
      Len := Length(psContents);
      if Len > 0 then
      begin
        for liLoop := 1 to Len do
        begin
          wChar := Swap(word(psContents[liLoop]));
          fs.WriteBuffer(wChar, 2);
        end;
      end;
    end;

  finally
    fs.Free;
  end;
end;


end.
