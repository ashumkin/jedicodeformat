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
    { delphi } Classes,
    { local } ConvertTypes, CodeReader, CodeWriter, BuildTokenList,
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
    fsConvertErrorMessage: string;

    fOnStatusMessage: TStatusMessageProc;

    //procedure SetSettings(const pcValue: TSettings);

  protected
    fbAbort: Boolean;
    fiCount: integer;

    // these are base class refs. this class's child will know what to insantiate
    fcReader: TCodeReader;
    fcWriter: TCodeWriter;

    procedure DoShowMessage(const psMessage: string);
    procedure SendStatusMessage(const ps: string);
    procedure FinalSummary;

    procedure DoConvertUnit;

    function OriginalFileName: string; virtual;

    { abstract factories called in the constructor. override these }
    function CreateReader: TCodeReader; virtual;
    function CreateWriter: TCodeWriter; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Convert; virtual;
    procedure Reset;

    //property Settings: TSettings read fcSettings write SetSettings;

    property OnStatusMessage: TStatusMessageProc
      read fOnStatusMessage write fOnStatusMessage;

    property YesAll: Boolean read fbYesAll write fbYesAll;
    property GuiMessages: Boolean read fbGuiMessages write fbGuiMessages;

    property TokenCount: integer read fiTokenCount;
    property ConvertError: boolean read fbConvertError;
    property ConvertErrorMessage: string read fsConvertErrorMessage;
  end;

implementation

uses
  { delphi } Windows, SysUtils, Dialogs, Controls,
  { local } SourceTokenList, fShowParseTree, JcfSettings,
  ObfuscateControl;


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

  //fcSettings := nil;
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

procedure TConverter.SendStatusMessage(const ps: string);
begin
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(ps);
end;

{ for failures, use console output, not showmessage in gui mode }
procedure TConverter.DoShowMessage(const psMessage: string);
begin
  if fbGuiMessages then
    ShowMessage(psMessage)
  else
    SendStatusMessage(psMessage);
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

procedure TConverter.FinalSummary;
var
  lsMessage: string;
begin
  if fiCount = 0 then
    lsMessage := 'Nothing done'
  else if fbAbort then
    lsMessage := 'Aborted after ' + IntToStr(fiCount) + ' files'
  else
    lsMessage := 'Finished processing ' + IntToStr(fiCount) + ' files';

  SendStatusMessage(lsMessage);

  Log.EmptyLine;
  Log.Write(lsMessage);
end;

procedure TConverter.Reset;
begin
  fbYesAll := False;
end;

procedure TConverter.DoConvertUnit;
var
  lcTokenList: TSourceTokenList;
begin
  //Assert(Settings <> nil);
  fbConvertError := False;
  fsConvertErrorMessage := '';

  try
    fcWriter.Clear;

    // turn test into tokens
    lcTokenList := fcTokeniser.BuildTokenList;
    try
      lcTokenList.SetXYPositions;
      fiTokenCount := lcTokenList.Count;

      // make a parse tree from it
      fcBuildParseTree.TokenList := lcTokenList;
      fcBuildParseTree.BuildParseTree;
    finally
      lcTokenList.Free;
    end;

    if fcBuildParseTree.ParseError then
    begin
      {!!! debug code }
      ShowMessage(fcBuildParseTree.ParseErrorMessage);
      ShowParseTree(fcBuildParseTree.Root);

      fbConvertError := True;
      fsConvertErrorMessage := fcBuildParseTree.ParseErrorMessage;
    end
    else
    begin
      // do the processes
      if Settings.Obfuscate.Enabled then
      begin
        Obfuscate(fcBuildParseTree.Root);
      end
      else
      begin
      end;

      fcWriter.Root := fcBuildParseTree.Root;
      fcWriter.WriteAll;
      fcWriter.Close;
    end;

    fcBuildParseTree.Clear;

  except
    on E: Exception do
    begin
      SendStatusMessage('Could not convert the unit: ' + E.Message);
      fbConvertError := True;
      fsConvertErrorMessage := E.Message;
    end;
  end;
end;

end.