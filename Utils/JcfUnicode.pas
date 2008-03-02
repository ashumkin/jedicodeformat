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
  FileContentType = (e8Bit, eUtf16LittleEndian, eUtf16BigEndian);

procedure ReadTextFile(const psFileName: string;
  out contents: WideString; out contentType: FileContentType);

procedure WriteTextFile(const psFileName: string;
  const psContents: WideString; const peContentType: FileContentType);


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
procedure ReadTextFile(const psFileName: string;
  out psContents: WideString; out peContentType: FileContentType);
var
  fs:     TFileStream;
  w:      word;
  ws:     WideString;
  contents8bit: string;
  liBytesRemaining: integer;
  liLoop: integer;
const
  Utf16LittleEndianMarker = $FEFF;
  Utf16BigEndianMarker    = $FFFE;

begin
  {open file}
  fs := TFileStream.Create(psFileName, fmOpenRead);
  try

    { the stream can contain unicode characters -
       we must check before parse }
    fs.Read(w, SizeOf(w));
    if ((w = Utf16LittleEndianMarker) or (w = Utf16BigEndianMarker)) then
    begin
      if (fs.Size > fs.Position) then
      begin
        // read it
        liBytesRemaining := fs.Size - fs.Position;
        SetLength(ws, liBytesRemaining div 2);
        fs.Read(ws[1], liBytesRemaining);

        if (w = Utf16BigEndianMarker) then
        begin
          peContentType := eUtf16BigEndian;

          // swap the bytes
          for liLoop := 1 to Length(ws) do
            ws[liLoop] := widechar(Swap(word(ws[liLoop])));
        end
        else
        begin
          peContentType := eUtf16LittleEndian;
        end;
      end;
    end
    else
    begin
      // it's just an 8-bit text file
      peContentType := e8Bit;

      // restore position
      fs.Seek(-SizeOf(w), soFromCurrent);

      // read the bytes into a string
      SetLength(contents8bit, fs.Size);
      if fs.Size > 0 then
      begin
        fs.ReadBuffer(contents8bit[1], fs.Size);
      end;

      // convert to wide char 
      psContents := contents8bit;
    end;

  finally
    // close the file
    fs.Free;
  end;
end;

procedure WriteTextFile(const psFileName: string;
  const psContents: WideString; const peContentType: FileContentType);
var
  fs: TFileStream;
  Len: Integer;
  lsContents: string;
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

    end;
         


  finally
    fs.Free;
  end;
end;


end.
