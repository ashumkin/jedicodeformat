unit StringsConverter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is StringsConverter, released May 2003.
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

interface

uses
  { delphi } Classes,
  { local } Converter, CodeReader, CodeWriter, StringsReader, StringsWriter;

type
  TStringsConverter = class(TConverter)
  private
    fcMessageStrings: TStrings;

    procedure SetInputStrings(const pcStrings: TStrings);
    procedure SetOutputStrings(const pcStrings: TStrings);
    procedure SetMessageStrings(const pcStrings: TStrings);

    function GetInputStrings: TStrings;
    function GetOutputStrings: TStrings;
    function GetMessageStrings: TStrings;

  protected
    function CreateReader: TCodeReader; override;
    function CreateWriter: TCodeWriter; override;
    function OriginalFileName: string; override;

    procedure SendStatusMessage(const psMessage: string); override;

  public
    constructor Create;

    procedure Convert; override;

    property InputStrings: TStrings read GetInputStrings write SetInputStrings;
    property OutputStrings: TStrings read GetOutputStrings write SetOutputStrings;
    property MessageStrings: TStrings read GetMessageStrings write SetMessageStrings;

  end;


implementation

{ TStringsConverter }


constructor TStringsConverter.Create;
begin
  inherited;
  fcMessageStrings := nil;
end;


procedure TStringsConverter.Convert;
begin
  DoConvertUnit;
end;

function TStringsConverter.CreateReader: TCodeReader;
begin
  Result := TStringsReader.Create;
end;

function TStringsConverter.CreateWriter: TCodeWriter;
begin
  Result := TStringsWriter.Create;
end;

function TStringsConverter.GetMessageStrings: TStrings;
begin
  Result := fcMessageStrings;
end;

function TStringsConverter.GetInputStrings: TStrings;
begin
  Result := (fcReader as TStringsReader).InputStrings;
end;

function TStringsConverter.GetOutputStrings: TStrings;
begin
  Result := (fcWriter as TStringsWriter).OutputStrings;
end;

function TStringsConverter.OriginalFileName: string;
begin
  Result := 'text';
end;

procedure TStringsConverter.SetMessageStrings(const pcStrings: TStrings);
begin
  fcMessageStrings := pcStrings;
end;

procedure TStringsConverter.SetInputStrings(const pcStrings: TStrings);
begin
  (fcReader as TStringsReader).InputStrings := pcStrings;
end;

procedure TStringsConverter.SetOutputStrings(const pcStrings: TStrings);
begin
  (fcWriter as TStringsWriter).OutputStrings := pcStrings;
end;

procedure TStringsConverter.SendStatusMessage(const psMessage: string);
begin
  if fcMessageStrings <> nil then
  begin
    fcMessageStrings.Add(psMessage);
  end;

end;

end.