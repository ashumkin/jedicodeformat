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
  { delphi design time } ToolsAPI,
  { local } Converter, CodeReader, CodeWriter, EditorReader, EditorWriter;

type

  TEditorConverter = class(TConverter)
  private
    fsCurrentUnitName: string;

    function EditorReader: TEditorReader;
    function EditorWriter: TEditorWriter;
  protected
    { abstract factories called in the constructor. override these }
    function CreateReader: TCodeReader; override;
    function CreateWriter: TCodeWriter; override;
    function OriginalFileName: string; override;

  public
    constructor Create;

    procedure ConvertUnit(const pciUnit: IOTASourceEditor);

    procedure AfterConvert;
  end;


implementation

uses
  { local }
  JcfLog, JcfRegistrySettings;

procedure TEditorConverter.AfterConvert;
begin
  FinalSummary;
  Log.CloseLog;

  if GetRegSettings.ViewLogAfterRun then
    GetRegSettings.ViewLog;
end;

procedure TEditorConverter.ConvertUnit(const pciUnit: IOTASourceEditor);
var
  lcBuffer: IOTAEditBuffer;
begin
  Assert(pciUnit <> nil);

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

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

  try
    { reader and writer need to know the unit to make a reader }
    EditorReader.Clear;
    EditorReader.SetEditorUnit(pciUnit);
    EditorWriter.SetEditorUnit(pciUnit);
    fsCurrentUnitName := lcBuffer.FileName;

    // now convert
    DoConvertUnit;

    fsCurrentUnitName := '';

    if not ConvertError then
    begin
      SendStatusMessage(lcBuffer.FileName, 'Formatted unit', -1, -1);
      Inc(fiConvertCount);
    end;

  finally
    EditorReader.SetEditorUnit(nil);
    EditorWriter.SetEditorUnit(nil);
  end;
end;

constructor TEditorConverter.Create;
begin
  inherited;

  // send them to the IDE message pane
  GuiMessages := False;
end;

function TEditorConverter.CreateReader: TCodeReader;
begin
  Result := TEditorReader.Create;
end;

function TEditorConverter.CreateWriter: TCodeWriter;
begin
  Result := TEditorWriter.Create;
end;

function TEditorConverter.EditorReader: TEditorReader;
begin
  Result := fcReader as TEditorReader;
  Assert(Result <> nil);
end;

function TEditorConverter.EditorWriter: TEditorWriter;
begin
  Result := fcWriter as TEditorWriter;
  Assert(Result <> nil);
end;

function TEditorConverter.OriginalFileName: string;
begin
  if fsCurrentUnitName <> '' then
    Result := fsCurrentUnitName
  else
    Result := 'IDE';
end;

end.