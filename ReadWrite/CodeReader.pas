{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is CodeReader.pas, released April 2000.
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

unit CodeReader;

{
{ Created AFS 27 November 1999
  abstract base class reader for both IDE and file
  defines the itnerface to get the code text

  The method is to first read the entire file into a string
  This is a textbook optimization - 1 read for the whole file
  instead of 1 per char. The file may be large
  (the largest file that ships with Delphi5, excel2000.pas, is 4Mb!!!!)
  but even this should fit into memory
  This technique is not optimised for files of that size,
  but hey, that code has got to be machine-generated anyway.
  Why would it need machine-reformatting?

  8 Jan 2K - the original code is now split into
  TCodeReader (base class) and TFileReader (read from file
  so that another subclass (TEditorReader) can be made for the IDE pluggin
  with the same interface

  Now called  TCodeReader not TReader to avoid a name clash with Classes.Reader

}

interface

type
  TCodeReader = class(TObject)
  private
    function GetBuffer: string;
    procedure SetBufferLength(const piValue: integer);

  protected
    { working vars }
    fsSource: string;
    fiSourceLength: integer;
    fiReadIndex, fiBufferLength: integer;

    fbHasRead: boolean;

    procedure ReadFromSource; virtual;

  public
    constructor Create; virtual;

    procedure Clear; virtual;

    procedure Consume; overload;
    procedure Consume(piChars: integer); overload;
    function ConsumeBuffer: string;

    function Current: char;
    function Last: char;

    procedure IncBuffer;
    procedure DecBuffer;

    function BufferCharsLeft(const piLength: integer): string;
    function BufferCharsRight(const piLength: integer): string;

    function EndOfFile: boolean;
    function BufferEndOfFile: boolean;

    property Buffer: string read GetBuffer;
    property BufferLength: integer read fiBufferLength write SetBufferLength;
  end;

implementation

uses
    {delphi } SysUtils;

{ TCodeReader }

constructor TCodeReader.Create;
begin
  inherited;
  Clear;
end;

procedure TCodeReader.Clear;
begin
  fsSource       := '';
  fiSourceLength := 0;

  fiReadIndex    := -1;
  fiBufferLength := 1;
  fbHasRead      := False;
end;


function TCodeReader.EndOfFile: boolean;
begin
  Result := (fiReadIndex > fiSourceLength);
end;

function TCodeReader.BufferEndOfFile: boolean;
begin
  Result := ((fiReadIndex + BufferLength - 1) >= fiSourceLength);
end;

{ make sure that the buffer is at least this length }
procedure TCodeReader.SetBufferLength(const piValue: integer);
begin
  if not fbHasRead then
    ReadFromSource;

  Assert(piValue >= 1);
  fiBufferLength := piValue;
end;

procedure TCodeReader.IncBuffer;
begin
  inc(fiBufferLength);

  { chop to the remaining length }
  if (fiReadIndex + fiBufferLength - 1) > fiSourceLength then
    fiBufferLength := fiSourceLength - fiReadIndex + 1;
end;

procedure TCodeReader.DecBuffer;
begin
  dec(fiBufferLength);
  if fiBufferLength < 1 then
    fiBufferLength := 1;
end;


procedure TCodeReader.Consume;
begin
  inc(fiReadIndex);
end;

procedure TCodeReader.Consume(piChars: integer);
begin
  Assert(piChars > 0);
  inc(fiReadIndex, piChars);
  dec(fiBufferLength, piChars);
  if fiBufferLength < 1 then
    fiBufferLength := 1;
end;

function TCodeReader.ConsumeBuffer: string;
begin
  Result := Buffer;
  Consume(BufferLength);
  BufferLength := 1;
end;

function TCodeReader.GetBuffer: string;
begin
  Result := Copy(fsSource, fiReadIndex, BufferLength);
end;

function TCodeReader.BufferCharsLeft(const piLength: integer): string;
begin
  Result := '';
  if fsSource = '' then
    exit;

  Result := Copy(fsSource, fiReadIndex, piLength);
end;

function TCodeReader.BufferCharsRight(const piLength: integer): string;
var
  liStart: integer;
begin
  Result := '';
  if fsSource = '' then
    exit;
  Assert(piLength <= BufferLength);

  liStart := (fiReadIndex + BufferLength - piLength);
  if liStart > fiSourceLength then
    Result := #0
  else
    Result := Copy(fsSource, liStart, piLength);
end;

function TCodeReader.Current: char;
begin
  if not fbHasRead then
    ReadFromSource;

  if (fiReadIndex > fiSourceLength) then
    Result := #0
  else
    Result := fsSource[fiReadIndex];
end;

function TCodeReader.Last: char;
var
  liIndex: integer;
begin
  liIndex := fiReadIndex + BufferLength - 1;

  if BufferLength < 0 then
    Result := #0
  else if (liIndex > fiSourceLength) or (liIndex < 1) then
    Result := #0
  else
    Result := fsSource[liIndex];
end;

procedure TCodeReader.ReadFromSource;
begin
  Assert(False, ClassName + ' must override TCodeReader.ReadFromSource');
end;

end.