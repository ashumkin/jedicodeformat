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
    { delphi } Classes, SysUtils,
    { local } ConvertTypes, ParseTreeNode,
    CodeReader, CodeWriter, BuildTokenList,
    BuildParseTree, JCFLog;


type
  TConverter = class(TObject)
  private
    fcTokeniser: TBuildTokenList;
    fcBuildParseTree: TBuildParseTree;

    { settings }
    //fcSettings: TSettings;

    fbYesAll, fbGuiMessages: boolean;
    fiTokenCount: integer;

    fbConvertError: boolean;
    fOnStatusMessage: TStatusMessageProc;

  protected
    fbAbort: Boolean;
    fiConvertCount: integer;

    // these are base class refs. this class's child will know what to insantiate
    fcReader: TCodeReader;
    fcWriter: TCodeWriter;

    { call this to display a parse error or other falure }
    procedure DoShowException(const pe: Exception);

    { call this to report the current state of the proceedings }
    procedure SendStatusMessage(const psFile, psMessage: string;
      const piY: integer = -1; const piX: integer = -1); virtual;
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

    //property Settings: TSettings read fcSettings write SetSettings;

    property OnStatusMessage: TStatusMessageProc
      read fOnStatusMessage write fOnStatusMessage;

    property YesAll: Boolean read fbYesAll write fbYesAll;
    property GuiMessages: Boolean read fbGuiMessages write fbGuiMessages;

    property TokenCount: integer read fiTokenCount;
    property ConvertError: boolean read fbConvertError;

    property Root: TParseTreeNode read GetRoot;
  end;

implementation

uses
  { delphi } Windows, Dialogs, Controls, Forms,
  { local } SourceTokenList, fShowParseTree, JcfSettings, JcfRegistrySettings,
  AllProcesses, Preprocessor, ParseError, fJcfErrorDisplay;


constructor TConverter.Create;
begin
  inherited;

  { state }
  fbGuiMessages := True;
  fbYesAll := False;

  { create owned objects }
  fcReader    := CreateReader;
  Assert(fcReader <> nil);

  fcWriter    := CreateWriter;
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

procedure TConverter.SendStatusMessage(const psFile, psMessage: string; const piY, piX: integer);
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

      lsText := lcParseError.Message + ' near ' + lcParseError.TokenMessage;
      if OriginalFileName <> '' then
        lsText := lsText + ' in ' + OriginalFileName;

      SendStatusMessage(OriginalFileName, lsText, lcParseError.YPosition, lcParseError.XPosition);
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
    lsMessage := 'Nothing done'
  else if fbAbort then
    lsMessage := 'Aborted after ' + DescribeFileCount(fiConvertCount)
  else if fiConvertCount > 1 then
    lsMessage := 'Finished processing ' + DescribeFileCount(fiConvertCount);

  SendStatusMessage('', lsMessage);

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

  //Assert(Settings <> nil);
  fbConvertError := False;

  try
    fcWriter.Clear;

    leOldCursor := Screen.Cursor;
    try
      // this can take a long time for large files
      Screen.Cursor := crHourGlass;

      // turn test into tokens
      lcTokenList := fcTokeniser.BuildTokenList;
      try
        fiTokenCount := lcTokenList.Count;

        lcTokenList.SetXYPositions;

        // remove conditional compilation stuph
        if FormatSettings.PreProcessor.Enabled then
          RemoveConditionalCompilation(lcTokenList);

          // make a parse tree from it
        fcBuildParseTree.TokenList := lcTokenList;

        try
          fcBuildParseTree.BuildParseTree;
        except
          on E: Exception do
          begin
            fbConvertError := True;
            DoShowException(E);
          end;
        end;
      finally
        if fbConvertError then
        begin
          { if there was a parse error, the rest of the unit was not parsed
           there may still be tokens in the list
           Free them or face a small but annoying memory leak. }
          lcTokenList.ClearAndFree;
        end;

        // should not be any tokens left
        Assert(lcTokenList.Count = 0);
        lcTokenList.Free;
      end;

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

    finally
      Screen.Cursor := leOldCursor;
    end;

  except
    on E: Exception do
    begin
      DoShowException(E);
      fbConvertError := True;
    end;
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
      Raise;
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