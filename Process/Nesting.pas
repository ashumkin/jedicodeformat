unit Nesting;

{ AFS 10 Jan 2002
  right, this is fairly generic code
  to store on each token nesting level info for a variety of indicators
  such as
  - begin end block nesting level
  - record case nesting level
  - case statement, try statment etc.
  - procedure nesting level

  Easier to set this up with a visitor and store it on a leaf node
  than the generate it on the fly
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Nesting, released May 2003.
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

uses Contnrs;

type

TNestingLevelType = (
  nlBlock, // generic code indent
  nlCaseSelector,
  nlRecordType,
  nlRecordVariantSection,
  nlProcedure,
  nlRoundBracket, nlSquareBracket,
  nlStatementLabel);

{ store a nesting level for one of the above enums
  a record-ish type}
TNestingLevelItem = class(TObject)
  private
    feItemType: TNestingLevelType;
    fiNestingLevel: integer;
  public

    property ItemType: TNestingLevelType read feItemType write feItemType;
    property NestingLevel: integer read fiNestingLevel write fiNestingLevel;
end;


{ a list of the items above }
TNestingLevelList = class(TObject)
  private
    fcList: TObjectList;

    function GetItem(const peItemType: TNestingLevelType; const pbCreate: boolean): TNestingLevelItem;
    function GetItems(const piIndex: integer): TNestingLevelItem;

  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    procedure Clear;

    function GetLevel(const peItemType: TNestingLevelType): integer;
    procedure IncLevel(const peItemType: TNestingLevelType);
    procedure DecLevel(const peItemType: TNestingLevelType);

    procedure Assign(const pcSource: TNestingLevelList);

    function FinalTest: string;
    function Total: integer;

    property Items[const piIndex: integer]: TNestingLevelItem read GetItems;

end;

implementation

uses SysUtils;

constructor TNestingLevelList.Create;
begin
  inherited;

  fcList := TObjectList.Create;
end;

destructor TNestingLevelList.Destroy;
begin
  FreeAndNil(fcList);
  inherited;
end;

function TNestingLevelList.Count: integer;
begin
  Result := fcList.Count;
end;

function TNestingLevelList.GetItem(const peItemType: TNestingLevelType; const pbCreate: boolean): TNestingLevelItem;
var
  liLoop: integer;
  lcItem: TNestingLevelItem;
begin
  Result := nil;

  for liLoop := 0 to Count - 1 do
  begin
    lcItem := TNestingLevelItem(fcList.Items[liLoop]);
    if lcItem.ItemType = peItemType then
    begin
      Result := lcItem;
      break;
    end;
  end;

  if (Result = nil) and pbCreate then
  begin
    // create
    Result := TNestingLevelItem.Create;
    Result.ItemType := peItemType;
    Result.NestingLevel := 0;

    fcList.Add(Result);
  end;
end;


procedure TNestingLevelList.DecLevel(const peItemType: TNestingLevelType);
var
  lcItem: TNestingLevelItem;
begin
  lcItem := GetItem(peItemType, True);
  lcItem.NestingLevel := lcItem.NestingLevel - 1;
end;


procedure TNestingLevelList.IncLevel(const peItemType: TNestingLevelType);
var
  lcItem: TNestingLevelItem;
begin
  lcItem := GetItem(peItemType, True);
  lcItem.NestingLevel := lcItem.NestingLevel + 1;
end;

function TNestingLevelList.GetLevel(const peItemType: TNestingLevelType): integer;
var
  lcItem: TNestingLevelItem;
begin
  lcItem := GetItem(peItemType, False);
  if lcItem = nil then
    Result := 0
  else
    Result := lcItem.NestingLevel;
end;                                               


{ at the end of it all, all should be back to zero }
function TNestingLevelList.FinalTest: string;
var
  liLoop: integer;
  lcItem: TNestingLevelItem;
begin
  Result := '';

  for liLoop := 0 to Count - 1 do
  begin
    lcItem := Items[liLoop];
    if lcItem.NestingLevel > 0 then
    begin
      Result := 'Final nesting level = ' + IntToStr(liLoop);
      break;
    end;
  end;
end;

procedure TNestingLevelList.Assign(const pcSource: TNestingLevelList);
var
  liLoop: integer;
  lcSource, lcNew: TNestingLevelItem;
begin

  Clear;

  if pcSource = nil then
    exit;

  for liLoop := 0 to pcSource.Count - 1 do
  begin
    lcSource := pcSource.Items[liLoop];

    lcNew := TNestingLevelItem.Create;
    lcNew.ItemType := lcSource.ItemType;
    lcNew.NestingLevel := lcSource.NestingLevel;

    fcList.Add(lcNew);
  end;

end;

procedure TNestingLevelList.Clear;
begin
  fcList.Clear;
end;

function TNestingLevelList.GetItems(const piIndex: integer): TNestingLevelItem;
begin
  Result := TNestingLevelItem(fcList.Items[piIndex]);
end;

function TNestingLevelList.Total: integer;
var
  liLoop: integer;
  lcItem: TNestingLevelItem;
begin

  Result := 0;;

  for liLoop := 0 to Count - 1 do
  begin
    lcItem := Items[liLoop];
    Result := Result +  lcItem.NestingLevel;
  end;
end;

end.