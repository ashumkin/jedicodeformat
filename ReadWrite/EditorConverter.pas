unit EditorConverter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is EditorConverter.pas, released January 2001.
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

{ AFS 12 Jan 2K
  Converter class for the IDE pluggin
}

interface

uses
  { delphi design time }
  ToolsAPI,
  { local }
  Converter, ConvertTypes;

type

  TEditorConverter = class(TObject)
  private
    { state }
    fOnStatusMessage: TStatusMessageProc;
    fsCurrentUnitName: string;
    fiConvertCount: integer;

    procedure SendStatusMessage(const psUnit, psMessage: string;
      const piY, piX: integer);

    function GetOnStatusMessage: TStatusMessageProc;
    procedure SetOnStatusMessage(const Value: TStatusMessageProc);

    function ReadFromIDE(const pcUnit: IOTASourceEditor): string;
    procedure WriteToIDE(const pcUnit: IOTASourceEditor; const psText: string);

    procedure FinalSummary;
    function OriginalFileName: string;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Convert(const pciUnit: IOTASourceEditor);

    procedure Clear;

    procedure BeforeConvert;
    procedure AfterConvert;

    property OnStatusMessage: TStatusMessageProc read GetOnStatusMessage write SetOnStatusMessage;
  end;


implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfLog, JcfDllExtern;

constructor TEditorConverter.Create;
begin
  inherited;

end;

destructor TEditorConverter.Destroy;
begin
  inherited;
end;

procedure TEditorConverter.Convert(const pciUnit: IOTASourceEditor);
var
  lcBuffer: IOTAEditBuffer;
  lsIn, lsOut: string;
begin
  Assert(pciUnit <> nil);

  { check for read-only  }
  pciUnit.QueryInterface(IOTAEditBuffer, lcBuffer);
  if pciUnit <> nil then
  begin
    lcBuffer := pciUnit as IOTAEditBuffer;
    if lcBuffer.IsReadOnly then
    begin
      SendStatusMessage(lcBuffer.FileName, 'Unit is read only. Cannot format ', -1, -1);
      exit;
    end;
  end;

  fsCurrentUnitName := lcBuffer.FileName;
  lsIn := ReadFromIDE(pciUnit);

  // now convert
  SetOnStatusMessage(SendStatusMessage);
  lsOut := Format(lsIn);

  fsCurrentUnitName := '';

  if not ConvertError then
  begin
    WriteToIDE(pciUnit, lsOut);
    SendStatusMessage(lcBuffer.FileName, 'Formatted unit', -1, -1);
    Inc(fiConvertCount);
  end;
end;

function TEditorConverter.ReadFromIDE(const pcUnit: IOTASourceEditor): string;
const
  // 10 kb at a time should do it
  BUF_SIZE = 10240;
 //BUF_SIZE = 120; // small for testing
var
  lciEditorReader: IOTAEditReader;
  lsBuf:  string;
  lpBuf:  pchar;
  liActualSize, liPos: integer;
  lbDone: boolean;
  //liLoopCount: integer;
begin
  { get a reader from the unit }
  Assert(pcUnit <> nil);
  lciEditorReader := pcUnit.CreateReader;
  Assert(lciEditorReader <> nil);

  Result := '';

  // read it all. Unfortunately the API dictates that we will work in chunks

  liPos := 0;
  //liLoopCount := 0;

  lbDone := False;

  while not lbDone do
  begin
    // clear the buffer
    SetLength(lsBuf, BUF_SIZE);
    lpBuf := pchar(lsBuf);
    FillChar(lpBuf^, BUF_SIZE, 0);

    // get some text into the buffer
    liActualSize := lciEditorReader.GetText(liPos, lpBuf, BUF_SIZE);

    // store it
    {WP: Do not add the entire lsBuf to fsSource, as in cases where the entire source is less
     than 10Kb in total, there will be junk in the last part of the buffer.
     If this is copied, it shows up as extraneous tokens in the token list
     after the end of the unit proper.
     This then causes an assertion failure in procedure DoConvertUnit in unit Converter.pas,
     When these extra tokens are found that were not consumed by BuildParseTree

     The way is to ensure that you only append as many characters as you've actually read (liActualSize bytes)
     from the buffer into the result. }
    Result := Result + Copy(lsBuf, 1, liActualSize);
      //WP: Changed from just adding lsBuf

    // more stuff to read after this?
    liPos  := liPos + liActualSize;
    lbDone := (liActualSize < BUF_SIZE);
    //inc(liLoopCount);
  end;
end;

procedure TEditorConverter.WriteToIDE(const pcUnit: IOTASourceEditor; const psText: string);
var
  lciEditorWriter: IOTAEditWriter;
//  liEndPos: integer;
begin
  if pcUnit = nil then
    exit;

  lciEditorWriter := pcUnit.CreateUndoableWriter;
  Assert(lciEditorWriter <> nil);

  if lciEditorWriter = nil then
    exit;

  { these next 2 steps should rather be done in one operation
    so as to be unitary in the undo history
    but I don't know how to do that, or if it is possible }

  { delete what's there }
  lciEditorWriter.DeleteTo(High(integer));
  { put the changed text in instead }
  lciEditorWriter.Insert(pchar(psText));

  { delete after the 'end.' }
  //liEndPos := PosOfLastSolidText(fsDestText);
  //lciEditorWriter.CurrentPos

  // ditch the interfaces
  lciEditorWriter := nil;
end;

procedure TEditorConverter.AfterConvert;
begin
  FinalSummary;
  Log.CloseLog;

  if GetRegSettings.ViewLogAfterRun then
    GetRegSettings.ViewLog;
end;

procedure TEditorConverter.Clear;
begin
  ClearFormat;
end;


function TEditorConverter.GetOnStatusMessage: TStatusMessageProc;
begin
  Result := fOnStatusMessage;
end;

function TEditorConverter.OriginalFileName: string;
begin
  if fsCurrentUnitName <> '' then
    Result := fsCurrentUnitName
  else
    Result := 'IDE';
end;

procedure TEditorConverter.SendStatusMessage(const psUnit,
  psMessage: string; const piY, piX: integer);
var
  lsUnit: string;
begin
  lsUnit := psUnit;
  if lsUnit = '' then
    lsUnit := OriginalFileName;

  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(lsUnit, psMessage, piY, piX);
end;

procedure TEditorConverter.SetOnStatusMessage(const Value: TStatusMessageProc);
begin
    fOnStatusMessage := Value;
end;

procedure TEditorConverter.FinalSummary;
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
  {
  else if fbAbort then
    lsMessage := 'Aborted after ' + DescribeFileCount(fiConvertCount)
  }
  else if fiConvertCount > 1 then
    lsMessage := 'Finished processing ' + DescribeFileCount(fiConvertCount)
  else
    lsMessage := '';

  if lsMessage <> '' then
    SendStatusMessage('', lsMessage, -1, -1);

  Log.EmptyLine;
  Log.Write(lsMessage);
end;

procedure TEditorConverter.BeforeConvert;
begin
  fiConvertCount := 0;
end;

end.
