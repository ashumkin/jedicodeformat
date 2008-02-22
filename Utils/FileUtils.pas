unit FileUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is FileUtils.pas, released October 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

{ this unit is a wrapper for platform-specific file fns
  IE a way to get rid of those portability warnings
  and a place to put the equivalent linux fns
  when we get there }

{$IFDEF WIN32}
uses {$WARNINGS OFF} FileCtrl {$WARNINGS ON};
{$ENDIF}


function FileIsReadOnly(const ps: string): boolean;

function SelectDirectory(const Caption: string; const Root: widestring;
  out Directory: string): boolean; overload;
function SelectDirectory(var Directory: string; Options: TSelectDirOpts;
  HelpCtx: longint): boolean; overload;

implementation



uses SysUtils;

{$IFDEF WIN32}

function FileIsReadOnly(const ps: string): boolean;
var
  liAttr: integer;
begin
  Assert(FileExists(ps));
{$WARNINGS OFF}
  liAttr := FileGetAttr(ps);
  Result := ((liAttr and faReadOnly) <> 0);
{$WARNINGS ON}
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
begin
  Result := FileCtrl.SelectDirectory(Caption, Root, Directory);
end;

function SelectDirectory(var Directory: string; Options: TSelectDirOpts;
  HelpCtx: Longint): Boolean;
begin
  Result := FileCtrl.SelectDirectory(Directory, Options, HelpCtx);
end;

{$ENDIF}

{$IFDEF LINUX}
  This bit will not compile under linux yet
  as the above win32 fns will not work there .
{$ENDIF}

end.
