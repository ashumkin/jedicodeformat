{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetSpaces.pas, released February 2001.
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

{ mostly spacing and line breaking +options }

unit SetSpaces;

interface

uses JCFSetBase, TokenType, SettingsStream;

type

  TSetSpaces = class(TSetBase)
  private
    fbTabsToSpaces: boolean;
    fbSpacesToTabs: boolean;
    fiSpacesForTab: integer;
    fiSpacesPerTab: integer;

    fbFixSpacing: boolean;
    fbSpaceBeforeClassHeritage: boolean;

    fiSpacesBeforeColonVar: integer;
    fiSpacesBeforeColonConst: integer;
    fiSpacesBeforeColonParam: integer;
    fiSpacesBeforeColonFn: integer;
    fiSpacesBeforeColonClassVar: integer;
    fiSpacesBeforeColonRecordField: integer;
    fiSpacesBeforeColonCaseLabel: integer;
    fiSpacesBeforeColonLabel: integer;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property TabsToSpaces: boolean read fbTabsToSpaces write fbTabsToSpaces;
    property SpacesToTabs: boolean read fbSpacesToTabs write fbSpacesToTabs;

    property SpacesPerTab: integer read fiSpacesPerTab write fiSpacesPerTab;
    property SpacesForTab: integer read fiSpacesForTab write fiSpacesForTab;

    property FixSpacing: boolean read fbFixSpacing write fbFixSpacing;

    property SpaceBeforeClassHeritage: boolean read fbSpaceBeforeClassHeritage write
      fbSpaceBeforeClassHeritage;

    property SpacesBeforeColonVar: integer read fiSpacesBeforeColonVar write fiSpacesBeforeColonVar;
    property SpacesBeforeColonConst: integer read fiSpacesBeforeColonConst write fiSpacesBeforeColonConst;
    property SpacesBeforeColonParam: integer read fiSpacesBeforeColonParam write fiSpacesBeforeColonParam;
    property SpacesBeforeColonFn: integer read fiSpacesBeforeColonFn write fiSpacesBeforeColonFn;
    property SpacesBeforeColonClassVar: integer read fiSpacesBeforeColonClassVar write fiSpacesBeforeColonClassVar;
    property SpacesBeforeColonRecordField: integer read fiSpacesBeforeColonRecordField write fiSpacesBeforeColonRecordField;
    property SpacesBeforeColonCaseLabel: integer read fiSpacesBeforeColonCaseLabel write fiSpacesBeforeColonCaseLabel;
    property SpacesBeforeColonLabel: integer read fiSpacesBeforeColonLabel write fiSpacesBeforeColonLabel;
  end;

implementation

const
  REG_TABS_TO_SPACES = 'TabsToSpaces';
  REG_SPACES_TO_TABS = 'SpacesToTabs';
  REG_SPACES_PER_TAB = 'SpacesPerTab';
  REG_SPACES_FOR_TAB = 'SpacesForTab';

  REG_FIX_SPACING    = 'FixSpacing';

  REG_SPACE_BEFORE_CLASS_HERITAGE = 'SpaceBeforeClassHeritage';

  REG_SPACES_BEFORE_COLON_VAR = 'SpacesBeforeColonVar';
  REG_SPACES_BEFORE_COLON_CONST = 'SpacesBeforeColonConst';
  REG_SPACES_BEFORE_COLON_PARAM = 'SpacesBeforeColonParam';
  REG_SPACES_BEFORE_COLON_FN = 'SpacesBeforeColonFn';
  REG_SPACES_BEFORE_COLON_CLASS_VAR = 'SpacesBeforeColonClassVar';
  REG_SPACES_BEFORE_COLON_RECORD_FIELD =  'SpacesBeforeColonRecordField';
  REG_SPACES_BEFORE_COLON_CASE_LABEL = 'SpacesBeforeColonCaseLabel';
  REG_SPACES_BEFORE_COLON_LABEL = 'SpacesBeforeColonLabel';

constructor TSetSpaces.Create;
begin
  inherited;
  SetSection('Spaces');
end;

procedure TSetSpaces.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbTabsToSpaces  := pcStream.Read(REG_TABS_TO_SPACES, True);
  fbSpacesToTabs  := pcStream.Read(REG_SPACES_TO_TABS, False);

  fiSpacesPerTab  := pcStream.Read(REG_SPACES_PER_TAB, 2);
  fiSpacesForTab  := pcStream.Read(REG_SPACES_FOR_TAB, 2);

  fbFixSpacing  := pcStream.Read(REG_FIX_SPACING, True);

  fbSpaceBeforeClassHeritage := pcStream.Read(REG_SPACE_BEFORE_CLASS_HERITAGE, False);

  fiSpacesBeforeColonVar := pcStream.Read(REG_SPACES_BEFORE_COLON_VAR, 0);
  fiSpacesBeforeColonConst := pcStream.Read(REG_SPACES_BEFORE_COLON_CONST, 0);
  fiSpacesBeforeColonParam := pcStream.Read(REG_SPACES_BEFORE_COLON_PARAM, 0);
  fiSpacesBeforeColonFn := pcStream.Read(REG_SPACES_BEFORE_COLON_FN, 0);
  fiSpacesBeforeColonClassVar := pcStream.Read(REG_SPACES_BEFORE_COLON_CLASS_VAR, 0);
  fiSpacesBeforeColonRecordField := pcStream.Read(REG_SPACES_BEFORE_COLON_RECORD_FIELD, 0);

  fiSpacesBeforeColonCaseLabel := pcStream.Read(REG_SPACES_BEFORE_COLON_CASE_LABEL, 0);
  fiSpacesBeforeColonLabel := pcStream.Read(REG_SPACES_BEFORE_COLON_LABEL, 0);
end;

procedure TSetSpaces.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_TABS_TO_SPACES, fbTabsToSpaces);
  pcOut.Write(REG_SPACES_TO_TABS, fbSpacesToTabs);
  pcOut.Write(REG_SPACES_PER_TAB, fiSpacesPerTab);
  pcOut.Write(REG_SPACES_FOR_TAB, fiSpacesForTab);

  pcOut.Write(REG_FIX_SPACING, fbFixSpacing);
  pcOut.Write(REG_SPACE_BEFORE_CLASS_HERITAGE, fbSpaceBeforeClassHeritage);

  pcOut.Write(REG_SPACES_BEFORE_COLON_VAR, fiSpacesBeforeColonVar);
  pcOut.Write(REG_SPACES_BEFORE_COLON_CONST, fiSpacesBeforeColonConst);
  pcOut.Write(REG_SPACES_BEFORE_COLON_PARAM, fiSpacesBeforeColonParam);
  pcOut.Write(REG_SPACES_BEFORE_COLON_FN, fiSpacesBeforeColonFn);
  pcOut.Write(REG_SPACES_BEFORE_COLON_CLASS_VAR, fiSpacesBeforeColonClassVar);

  pcOut.Write(REG_SPACES_BEFORE_COLON_RECORD_FIELD, fiSpacesBeforeColonRecordField);

  pcOut.Write(REG_SPACES_BEFORE_COLON_CASE_LABEL, fiSpacesBeforeColonCaseLabel);
  pcOut.Write(REG_SPACES_BEFORE_COLON_LABEL, fiSpacesBeforeColonLabel);
end;


end.