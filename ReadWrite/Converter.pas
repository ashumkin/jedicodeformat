{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Converter.pas, released April 2000.
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

unit Converter;

{ AFS 28 Nov 1999
  I don't want the controlling logic to be in a form
  so this class has it

  AFS 11 Jan 2K now a base class - file converter & editor converter are the
  real subclasses for reading from disk and IDE respectively
}

interface

uses
  { delphi } SysUtils,
  { local } ConvertTypes, ParseTreeNode,
  CodeReader, CodeWriter, BuildTokenList,
  BuildParseTree, JCFLog;

type
  TConverter = class(TObject)
  private
    fcTokeniser: TBuildTokenList;
    fcBuildParseTree: TBuildParseTree;

    fbYesAll, fbGuiMessages: boolean;
    fiTokenCount: integer;

    fbConvertError: boolean;
    fOnStatusMessage: TStatusMessageProc;

  protected
    fbAbort: boolean;
    fiConvertCount: integer;

    // these are base class refs. this class's child will know what to insantiate
    fcReader: TCodeReader;
    fcWriter: TCodeWriter;

    { call this to display a parse error or other falure }
    procedure DoShowException(const pe: Exception);

    { call this to report the current state of the proceedings }
    procedure SendStatusMessage(const psFile, psMessage: string;
      const piY, piX: integer); virtual;
    { last thing }
    procedure FinalSummary;

    procedure DoConvertUnit;
    function GetRoot: TParseTreeNode;

    function OriginalFileName: string; virtual;

    { abstract factories called in the constructor. override these }
    function CreateReader: TCodeReader; virtual;
    function CreateWriter: TCodeWriter; virtual;

    { this does the reformatting. Virtual method so can be overriden for testing }
    procedure ApplyProcesses; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Convert; virtual;
    procedure Clear; virtual;

    procedure BeforeConvert;

    property OnStatusMessage: TStatusMessageProc Read fOnStatusMessage
      Write fOnStatusMessage;

    property YesAll: boolean Read fbYesAll Write fbYesAll;
    property GuiMessages: boolean Read fbGuiMessages Write fbGuiMessages;

    property TokenCount: integer Read fiTokenCount;
    property ConvertError: boolean Read fbConvertError;

    property Root: TParseTreeNode Read GetRoot;
  end;

implementation

uses
  { delphi }
  Controls, Forms,
  { local }
  SourceTokenList, fShowParseTree, JcfSettings, JcfRegistrySettings,
  AllProcesses, ParseError, fJcfErrorDisplay, PreProcessorParseTree;


constructor TConverter.Create;
begin
  inherited;

  { state }
  fbGuiMessages := true;
  fbYesAll      := False;

  { create owned objects }
  fcReader := CreateReader;
  Assert(fcReader <> nil);

  fcWriter := CreateWriter;
  Assert(fcWriter <> nil);

  fcTokeniser := TBuildTokenList.Create;

  fcBuildParseTree := TBuildParseTree.Create;

  { wire them together }
  fcTokeniser.Reader := fcReader;
end;

destructor TConverter.Destroy;
begin
  FreeAndNil(fcReader);
  FreeAndNil(fcWriter);
  FreeAndNil(fcTokeniser);
  FreeAndNil(fcBuildParseTree);

  inherited;
end;

procedure TConverter.Convert;
begin
  Assert(False, ClassName + ' Must override TConverter.Convert');
end;

function TConverter.OriginalFileName: string;
begin
  Assert(False, ClassName + ' Must override TConverter.OriginalFileName');
end;


function TConverter.CreateReader: TCodeReader;
begin
  Assert(False, ClassName + ' Must override TConverter.CreateReader');
  Result := nil;
end;

function TConverter.CreateWriter: TCodeWriter;
begin
  Assert(False, ClassName + ' Must override TConverter.CreateWriter');
  Result := nil;
end;

procedure TConverter.SendStatusMessage(const psFile, psMessage: string;
  const piY, piX: integer);
var
  lsFile: string;
begin
  if Assigned(fOnStatusMessage) then
  begin
    lsFile := psFile;
    if lsFile = '' then
      // process doesn't know the file name? we do
      lsFile := OriginalFileName;

    fOnStatusMessage(lsFile, psMessage, piY, piX);
  end;
end;

{ for failures, use console output, not showmessage in gui mode }
procedure TConverter.DoShowException(const pe: Exception);
var
  lcParseError: TEParseError;
  lsText: string;
begin
  if fbGuiMessages then
  begin
    if (pE is TEParseError) then
    begin
      lcParseError := TEParseError(pE);
      lcParseError.FileName := OriginalFileName;
    end;

    ShowExceptionDialog(pe);
  end
  else
  begin
    if (pE is TEParseError) then
    begin
      lcParseError := TEParseError(pE);

      lsText := lcParseError.Message + ' near "' + lcParseError.TokenMessage + '"';
      if OriginalFileName <> '' then
        lsText := lsText + ' in ' + OriginalFileName;

      SendStatusMessage(OriginalFileName, lsText, lcParseError.YPosition,
        lcParseError.XPosition);
    end
    else
    begin
      SendStatusMessage(OriginalFileName, pe.Message, -1, -1);
    end;
  end;
end;


(*
procedure TConverter.SetSettings(const pcValue: TSettings);
begin
  fcSettings := pcValue;

  { tell the owned objects about it }
  fcProcess.Settings   := pcValue;
  fcTokeniser.Settings := pcValue;
  fcLog.Settings       := pcValue;
end;
*)

function DescribeFileCount(const piCount: integer): string;
begin
  if piCount = 1 then
    Result := '1 file'
  else
    Result := IntToStr(piCount) + ' files';
end;

procedure TConverter.FinalSummary;
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
    lsMessage := 'Finished processing ' + DescribeFileCount(fiConvertCount);

  SendStatusMessage('', lsMessage, -1, -1);

  Log.EmptyLine;
  Log.Write(lsMessage);
end;

procedure TConverter.Clear;
begin
  fbYesAll := False;

  fcReader.Clear;
  fcWriter.Clear;

  fiConvertCount := 0;
end;

procedure TConverter.DoConvertUnit;
var
  lcTokenList: TSourceTokenList;
  leOldCursor: TCursor;
begin
  fbConvertError := False;

  fcWriter.Clear;

  leOldCursor := Screen.Cursor;
  try { finally normal cursor }

    // this can take a long time for large files
    Screen.Cursor := crHourGlass;

    // turn text into tokens
    lcTokenList := fcTokeniser.BuildTokenList;
    try   { finally free the list  }
      try { show exceptions }
        fiTokenCount := lcTokenList.Count;

        lcTokenList.SetXYPositions;

        // remove conditional compilation stuph
        if FormatSettings.PreProcessor.Enabled then
          RemoveConditionalCompilation(lcTokenList);

          // make a parse tree from it
        fcBuildParseTree.TokenList := lcTokenList;
        fcBuildParseTree.BuildParseTree;

      except
        on E: Exception do
        begin
          fbConvertError := True;
          DoShowException(E);
        end;
      end;

      if fbConvertError then
      begin
        { if there was a parse error, the rest of the unit was not parsed
         there may still be tokens in the list
         Free them or face a small but annoying memory leak. }
        lcTokenList.Clear;
      end;

      // should not be any tokens left
      Assert(lcTokenList.Count = 0, 'Surplus tokens');

    finally
      lcTokenList.Free;
    end;

    try { show exception }
        // show the parse tree?
      if (GetRegSettings.ShowParseTreeOption = eShowAlways) or
        ((GetRegSettings.ShowParseTreeOption = eShowOnError) and fbConvertError) then
      begin
        if fcBuildParseTree.Root <> nil then
          ShowParseTree(fcBuildParseTree.Root);
      end;

      if not fbConvertError then
      begin
        // do the processes
        ApplyProcesses;

        fcWriter.Root := fcBuildParseTree.Root;
        fcWriter.WriteAll;
        fcWriter.Close;
      end;

      fcBuildParseTree.Clear;

    except
      on E: Exception do
      begin
        DoShowException(E);
        fbConvertError := True;
      end;
    end;

  finally
    Screen.Cursor := leOldCursor;
  end;

end;


procedure TConverter.ApplyProcesses;
var
  lcProcess: TAllProcesses;
begin
  lcProcess := TAllProcesses.Create;
  try
    lcProcess.OnMessage := SendStatusMessage;

    try
      lcProcess.Execute(fcBuildParseTree.Root);
    except
      ShowParseTree(fcBuildParseTree.Root);
      raise;
    end;
  finally
    lcProcess.Free;
  end;

end;

function TConverter.GetRoot: TParseTreeNode;
begin
  Result := fcBuildParseTree.Root;
end;

procedure TConverter.BeforeConvert;
begin
  fiConvertCount := 0;
end;


end.
