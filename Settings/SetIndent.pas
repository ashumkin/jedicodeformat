{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetIndent.pas, released April 2000.
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

unit SetIndent;

interface

uses JCFSetBase, SettingsStream;

type

  TSetIndent = class(TSetBase)
  private
    fiIndentSpaces: integer;
    fiFirstLevelIndent: integer;
    fbHasFirstLevelIndent: Boolean;

    fbIndentGlobals: boolean;
    fbIndentClasses: boolean;
    fbIndentProcedures: boolean;
    fbIndentBeginEnd: Boolean;

    fiIndentBeginEndSpaces: integer;

    fbKeepCommentsWithCodeInProcs: Boolean;
    fbKeepCommentsWithCodeInGlobals: Boolean;
    fbKeepCommentsWithCodeInClassDef: Boolean;
    fbBorlandCaseIndent: Boolean;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    function SpacesForIndentLevel(const piLevel: integer): integer;

    property IndentSpaces: integer read fiIndentSpaces write fiIndentSpaces;
    { first level can be indented differently }
    property FirstLevelIndent: integer read fiFirstLevelIndent write fiFirstLevelIndent;
    property HasFirstLevelIndent: boolean read fbHasFirstLevelIndent write fbHasFirstLevelIndent;

    property IndentGlobals: boolean read fbIndentGlobals write fbIndentGlobals;
    property IndentClasses: boolean read fbIndentClasses write fbIndentClasses;
    property IndentProcedures: boolean read fbIndentProcedures write fbIndentProcedures;

    property IndentBeginEnd: Boolean read fbIndentBeginEnd write fbIndentBeginEnd;
    property IndentBeginEndSpaces: integer read fiIndentBeginEndSpaces write fiIndentBeginEndSpaces;

    property KeepCommentsWithCodeInProcs: Boolean
      read fbKeepCommentsWithCodeInProcs write fbKeepCommentsWithCodeInProcs;
    property KeepCommentsWithCodeInGlobals: Boolean
      read fbKeepCommentsWithCodeInGlobals write fbKeepCommentsWithCodeInGlobals;
    property KeepCommentsWithCodeInClassDef: Boolean
      read fbKeepCommentsWithCodeInClassDef write fbKeepCommentsWithCodeInClassDef;

    property BorlandCaseIndent: boolean read fbBorlandCaseIndent write fbBorlandCaseIndent;

  end;

implementation

const
  REG_INDENTATION_SPACES = 'IndentationSpaces';

  REG_FIRST_LEVEL_INDENT = 'FirstLevelIndent';
  REG_HAS_FIRST_LEVEL_INDENT = 'HasFirstLevelIndent';

  REG_INDENT_GLOBALS    = 'IndentGlobals';
  REG_INDENT_CLASSES    = 'IndentClasses';
  REG_INDENT_PROCEDURES = 'IndentProcedures';

  REG_INDENT_BEGIN_END = 'IndentBeginEnd';
  REG_INDENT_BEGIN_END_SPACES = 'IndentbeginEndSpaces';

  REG_KEEP_COMMENTS_WTH_CODE_CLASS_DEF = 'KeepCommentsWithCodeInClassDef';
  REG_KEEP_COMMENTS_WTH_CODE_IN_PROCS = 'KeepCommentsWithCodeInProcs';
  REG_KEEP_COMMENTS_WTH_CODE_IN_GLOBALS = 'KeepCommentsWithCodeInGlobals';

  REG_BORLAND_CASE_INDENT = 'BorlandCaseIndent';

constructor TSetIndent.Create;
begin
  inherited;
  SetSection('Indent');
end;

procedure TSetIndent.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fiIndentSpaces := pcStream.Read(REG_INDENTATION_SPACES, 2);

  fiFirstLevelIndent := pcStream.Read(REG_FIRST_LEVEL_INDENT, 2);
  fbHasFirstLevelIndent := pcStream.Read(REG_HAS_FIRST_LEVEL_INDENT, False);

  fbIndentGlobals    := pcStream.Read(REG_INDENT_GLOBALS, True);
  fbIndentClasses    := pcStream.Read(REG_INDENT_CLASSES, True);
  fbIndentProcedures := pcStream.Read(REG_INDENT_PROCEDURES, True);

  fbIndentBeginEnd  := pcStream.Read(REG_INDENT_BEGIN_END, False);
  fiIndentBeginEndSpaces := pcStream.Read(REG_INDENT_BEGIN_END_SPACES, 1);

  fbKeepCommentsWithCodeInGlobals := pcStream.Read(REG_KEEP_COMMENTS_WTH_CODE_IN_GLOBALS, True);
  fbKeepCommentsWithCodeInProcs := pcStream.Read(REG_KEEP_COMMENTS_WTH_CODE_IN_PROCS, True);
  fbKeepCommentsWithCodeInClassDef := pcStream.Read(REG_KEEP_COMMENTS_WTH_CODE_CLASS_DEF, True);

  fbBorlandCaseIndent := pcStream.Read(REG_BORLAND_CASE_INDENT, True);
end;

procedure TSetIndent.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_INDENTATION_SPACES, fiIndentSpaces);

  pcOut.Write(REG_FIRST_LEVEL_INDENT, fiFirstLevelIndent);
  pcOut.Write(REG_HAS_FIRST_LEVEL_INDENT, fbHasFirstLevelIndent);

  pcOut.Write(REG_INDENT_GLOBALS, fbIndentGlobals);
  pcOut.Write(REG_INDENT_CLASSES, fbIndentClasses);
  pcOut.Write(REG_INDENT_PROCEDURES, fbIndentProcedures);

  pcOut.Write(REG_KEEP_COMMENTS_WTH_CODE_IN_GLOBALS, fbKeepCommentsWithCodeInGlobals);
  pcOut.Write(REG_KEEP_COMMENTS_WTH_CODE_IN_PROCS, fbKeepCommentsWithCodeInProcs);
  pcOut.Write(REG_KEEP_COMMENTS_WTH_CODE_CLASS_DEF, fbKeepCommentsWithCodeInClassDef);

  pcOut.Write(REG_INDENT_BEGIN_END, fbIndentBeginEnd);
  pcOut.Write(REG_INDENT_BEGIN_END_SPACES, fiIndentBeginEndSpaces);

  pcOut.Write(REG_BORLAND_CASE_INDENT, fbBorlandCaseIndent);
end;

function TSetIndent.SpacesForIndentLevel(const piLevel: integer): integer;
begin
  if piLevel <= 0 then
    Result := 0
  else if HasFirstLevelIndent then
    Result := FirstLevelIndent + ((piLevel - 1) * IndentSpaces)
  else
    Result := IndentSpaces * piLevel;
end;


end.