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

uses JCFSetBase, SettingsStream, SettingsTypes;

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

    fiMaxSpacesInCode: integer;
    fbUseMaxSpacesInCode: boolean;

    feSpaceForOperator: TTriOptionStyle;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property TabsToSpaces: boolean Read fbTabsToSpaces Write fbTabsToSpaces;
    property SpacesToTabs: boolean Read fbSpacesToTabs Write fbSpacesToTabs;

    property SpacesPerTab: integer Read fiSpacesPerTab Write fiSpacesPerTab;
    property SpacesForTab: integer Read fiSpacesForTab Write fiSpacesForTab;

    property FixSpacing: boolean Read fbFixSpacing Write fbFixSpacing;

    property SpaceBeforeClassHeritage: boolean
      Read fbSpaceBeforeClassHeritage Write fbSpaceBeforeClassHeritage;

    property SpacesBeforeColonVar: integer Read fiSpacesBeforeColonVar
      Write fiSpacesBeforeColonVar;
    property SpacesBeforeColonConst: integer
      Read fiSpacesBeforeColonConst Write fiSpacesBeforeColonConst;
    property SpacesBeforeColonParam: integer
      Read fiSpacesBeforeColonParam Write fiSpacesBeforeColonParam;
    property SpacesBeforeColonFn: integer Read fiSpacesBeforeColonFn
      Write fiSpacesBeforeColonFn;
    property SpacesBeforeColonClassVar: integer
      Read fiSpacesBeforeColonClassVar Write fiSpacesBeforeColonClassVar;
    property SpacesBeforeColonRecordField: integer
      Read fiSpacesBeforeColonRecordField Write fiSpacesBeforeColonRecordField;
    property SpacesBeforeColonCaseLabel: integer
      Read fiSpacesBeforeColonCaseLabel Write fiSpacesBeforeColonCaseLabel;
    property SpacesBeforeColonLabel: integer
      Read fiSpacesBeforeColonLabel Write fiSpacesBeforeColonLabel;

    property MaxSpacesInCode: integer Read fiMaxSpacesInCode Write fiMaxSpacesInCode;
    property UseMaxSpacesInCode: boolean Read fbUseMaxSpacesInCode Write fbUseMaxSpacesInCode;

    property SpaceForOperator: TTriOptionStyle read feSpaceForOperator write feSpaceForOperator;

  end;

implementation

const
  SET_TABS_TO_SPACES = 'TabsToSpaces';
  SET_SPACES_TO_TABS = 'SpacesToTabs';
  SET_SPACES_PER_TAB = 'SpacesPerTab';
  SET_SPACES_FOR_TAB = 'SpacesForTab';

  SET_FIX_SPACING = 'FixSpacing';

  SET_SPACE_BEFORE_CLASS_HERITAGE = 'SpaceBeforeClassHeritage';

  SET_SPACES_BEFORE_COLON_VAR   = 'SpacesBeforeColonVar';
  SET_SPACES_BEFORE_COLON_CONST = 'SpacesBeforeColonConst';
  SET_SPACES_BEFORE_COLON_PARAM = 'SpacesBeforeColonParam';
  SET_SPACES_BEFORE_COLON_FN    = 'SpacesBeforeColonFn';
  SET_SPACES_BEFORE_COLON_CLASS_VAR = 'SpacesBeforeColonClassVar';
  SET_SPACES_BEFORE_COLON_RECORD_FIELD = 'SpacesBeforeColonRecordField';
  SET_SPACES_BEFORE_COLON_CASE_LABEL = 'SpacesBeforeColonCaseLabel';
  SET_SPACES_BEFORE_COLON_LABEL = 'SpacesBeforeColonLabel';

  SET_MAX_SPACES_IN_CODE     = 'MaxSpacesInCode';
  SET_USE_MAX_SPACES_IN_CODE = 'UseMaxSpacesInCode';

  SET_SPACE_FOR_OPERATOR = 'SpaceForOperator';

constructor TSetSpaces.Create;
begin
  inherited;
  SetSection('Spaces');
end;

procedure TSetSpaces.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbTabsToSpaces := pcStream.Read(SET_TABS_TO_SPACES, True);
  fbSpacesToTabs := pcStream.Read(SET_SPACES_TO_TABS, False);

  fiSpacesPerTab := pcStream.Read(SET_SPACES_PER_TAB, 2);
  fiSpacesForTab := pcStream.Read(SET_SPACES_FOR_TAB, 2);

  fbFixSpacing := pcStream.Read(SET_FIX_SPACING, True);

  fbSpaceBeforeClassHeritage := pcStream.Read(SET_SPACE_BEFORE_CLASS_HERITAGE, False);

  fiSpacesBeforeColonVar   := pcStream.Read(SET_SPACES_BEFORE_COLON_VAR, 0);
  fiSpacesBeforeColonConst := pcStream.Read(SET_SPACES_BEFORE_COLON_CONST, 0);
  fiSpacesBeforeColonParam := pcStream.Read(SET_SPACES_BEFORE_COLON_PARAM, 0);
  fiSpacesBeforeColonFn    := pcStream.Read(SET_SPACES_BEFORE_COLON_FN, 0);
  fiSpacesBeforeColonClassVar := pcStream.Read(SET_SPACES_BEFORE_COLON_CLASS_VAR, 0);
  fiSpacesBeforeColonRecordField :=
    pcStream.Read(SET_SPACES_BEFORE_COLON_RECORD_FIELD, 0);

  fiSpacesBeforeColonCaseLabel := pcStream.Read(SET_SPACES_BEFORE_COLON_CASE_LABEL, 0);
  fiSpacesBeforeColonLabel     := pcStream.Read(SET_SPACES_BEFORE_COLON_LABEL, 0);

  fiMaxSpacesInCode    := pcStream.Read(SET_MAX_SPACES_IN_CODE, 2);
  fbUseMaxSpacesInCode := pcStream.Read(SET_USE_MAX_SPACES_IN_CODE, False);

  feSpaceForOperator := TTriOptionStyle(pcStream.Read(SET_SPACE_FOR_OPERATOR, Ord(eAlways)));
end;

procedure TSetSpaces.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(SET_TABS_TO_SPACES, fbTabsToSpaces);
  pcOut.Write(SET_SPACES_TO_TABS, fbSpacesToTabs);
  pcOut.Write(SET_SPACES_PER_TAB, fiSpacesPerTab);
  pcOut.Write(SET_SPACES_FOR_TAB, fiSpacesForTab);

  pcOut.Write(SET_FIX_SPACING, fbFixSpacing);
  pcOut.Write(SET_SPACE_BEFORE_CLASS_HERITAGE, fbSpaceBeforeClassHeritage);

  pcOut.Write(SET_SPACES_BEFORE_COLON_VAR, fiSpacesBeforeColonVar);
  pcOut.Write(SET_SPACES_BEFORE_COLON_CONST, fiSpacesBeforeColonConst);
  pcOut.Write(SET_SPACES_BEFORE_COLON_PARAM, fiSpacesBeforeColonParam);
  pcOut.Write(SET_SPACES_BEFORE_COLON_FN, fiSpacesBeforeColonFn);
  pcOut.Write(SET_SPACES_BEFORE_COLON_CLASS_VAR, fiSpacesBeforeColonClassVar);

  pcOut.Write(SET_SPACES_BEFORE_COLON_RECORD_FIELD, fiSpacesBeforeColonRecordField);

  pcOut.Write(SET_SPACES_BEFORE_COLON_CASE_LABEL, fiSpacesBeforeColonCaseLabel);
  pcOut.Write(SET_SPACES_BEFORE_COLON_LABEL, fiSpacesBeforeColonLabel);

  pcOut.Write(SET_MAX_SPACES_IN_CODE, fiMaxSpacesInCode);
  pcOut.Write(SET_USE_MAX_SPACES_IN_CODE, fbUseMaxSpacesInCode);

  pcOut.Write(SET_SPACE_FOR_OPERATOR, ord(feSpaceForOperator));
end;


end.
