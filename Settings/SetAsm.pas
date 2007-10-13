unit SetAsm;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetAsm.pas, released September 2007.
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

uses
  JCFSetBase,
  SettingsTypes, SettingsStream, IntList;

type

  TSetAsm = class(TSetBase)
  private
    feCapitalisation: TCapitalisationType;
    fiBreaksAfterLabel: integer;
    fcIndents: TIntList;
    fbBreaksAfterLabelEnabled: boolean;
    fbIndentsEnabled: boolean;

    function GetIndent(const index: integer): integer;
    procedure SetIndent(const index: integer; const Value: integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Capitalisation: TCapitalisationType Read feCapitalisation Write feCapitalisation;
    property BreaksAfterLabel: integer read fiBreaksAfterLabel write fiBreaksAfterLabel;
    property Indents[const index: integer]: integer read GetIndent write SetIndent;
    property BreaksAfterLabelEnabled: boolean Read fbBreaksAfterLabelEnabled Write fbBreaksAfterLabelEnabled;
    property IndentsEnabled: boolean Read fbIndentsEnabled Write fbIndentsEnabled;
 end;

implementation

uses
  SysUtils;

const
  REG_CAPS = 'Caps';
  REG_BREAKS_AFTER_LABEL = 'BreaksAfterLabel';
  REG_INDENT_LEVEL = 'Indent_';
  REG_BREAKS_AFTER_LABEL_ENABLED = 'BreaksAfterLabelEnabled';
  REG_INDENTS_ENABLED = 'IndentsEnabled';

const
  LAST_INDENT_ITEM = 5;

{ constuctor and destructor }

constructor TSetAsm.Create;
begin
  inherited;
  SetSection('Asm');

   fcIndents := TIntList.Create;
end;

destructor TSetAsm.Destroy;
begin
  FreeAndNil(fcIndents);
end;

{ properties }

function TSetAsm.GetIndent(const index: integer): integer;
begin
  // six values
  if (index < 0) or (index > LAST_INDENT_ITEM) then
  begin
    Result := 0;
  end
  else
  begin
    Result := fcIndents.Items[index];
  end;
end;

procedure TSetAsm.SetIndent(const index: integer; const Value: integer);
begin
  if (index >= 0) and (index <= LAST_INDENT_ITEM) then
    fcIndents.Items[Index] := value;
end;

procedure TSetAsm.ReadFromStream(const pcStream: TSettingsInput);
var
  liLoop: integer;
  liValue: integer;
begin
  Assert(pcStream <> nil);


  feCapitalisation := TCapitalisationType(pcStream.Read(REG_CAPS, Ord(ctLeaveAlone)));
  fiBreaksAfterLabel := pcStream.Read(REG_BREAKS_AFTER_LABEL, 2);

  fbBreaksAfterLabelEnabled := pcStream.Read(REG_BREAKS_AFTER_LABEL_ENABLED, True);
  fbIndentsEnabled := pcStream.Read(REG_INDENTS_ENABLED, True);

  fcIndents.Clear;
  for liLoop := 0 to LAST_INDENT_ITEM do
  begin
    liValue := pcStream.Read(REG_INDENT_LEVEL + IntToStr(liLoop), 0);
    fcIndents.Add(liValue);
  end;

end;

procedure TSetAsm.WriteToStream(const pcOut: TSettingsOutput);
var
  liLoop: integer;
  liMax: integer;
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_CAPS, Ord(feCapitalisation));
  pcOut.Write(REG_BREAKS_AFTER_LABEL, fiBreaksAfterLabel);

  pcOut.Write(REG_BREAKS_AFTER_LABEL_ENABLED, fbBreaksAfterLabelEnabled);
  pcOut.Write(REG_INDENTS_ENABLED, fbIndentsEnabled);

  liMax := LAST_INDENT_ITEM;
  if liMax > (fcIndents.Count - 1) then
    liMax := fcIndents.Count - 1;
  
  for liLoop := 0 to liMax do
  begin
    pcOut.Write(REG_INDENT_LEVEL + IntToStr(liLoop),  fcIndents.Items[liLoop]);
  end;
    
end;

end.
