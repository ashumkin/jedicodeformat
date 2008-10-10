unit CommandLineConstants;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is CommandLineConstants, released August 2008.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  JclAnsiStrings,
  VersionConsts;

const
  ABOUT_COMMANDLINE =
    'JEDI Code Format V' + PROGRAM_VERSION + AnsiLineBreak +
    ' ' + PROGRAM_DATE + AnsiLineBreak +
    ' A Delphi Object-Pascal Source code formatter' + AnsiLineBreak +
    ' A GUI version of this program is also available' + AnsiLineBreak +
    ' Latest version at ' + PROGRAM_HOME_PAGE + AnsiLineBreak + AnsiLineBreak +
    'Syntax: jcf [options] path/filename ' + AnsiLineBreak +
    ' Parameters to the command-line program: ' + AnsiLineBreak + AnsiLineBreak +

    ' Mode of operation: ' + AnsiLineBreak +
    ' -obfuscate Obfuscate mode or ' + AnsiLineBreak +
    ' -clarify Clarify mode' + AnsiLineBreak +
    '   When neither is specified, registry setting will be used.' + AnsiLineBreak +
    '   This normally means clarify.' + AnsiLineBreak + AnsiLineBreak +

    ' Mode of source: ' + AnsiLineBreak +
    ' -F Format a file. The file name must be specified.' + AnsiLineBreak +
    ' -D Format a directory. The directory name must be specified.' + AnsiLineBreak +
    ' -R Format a directory tree. The root directory name must be specified.' +
    AnsiLineBreak +
    '  When no file mode is specified, registry setting will be used.' +
    AnsiLineBreak + AnsiLineBreak +

    ' Mode of output: ' + AnsiLineBreak +
    ' -inplace change the source file without backup' + AnsiLineBreak +
    ' -out output to a new file' + AnsiLineBreak +
    ' -backup change the file and leave the original file as a backup' + AnsiLineBreak +
    '  If no output mode is specified, registry setting will be used.' +
    AnsiLineBreak + AnsiLineBreak +

    ' Other options: ' + AnsiLineBreak +
    ' -config=filename  To specify a named configuration file' + AnsiLineBreak +
    ' -y Overwrite files without confirmation.' + AnsiLineBreak +
    ' -? Display this help' + AnsiLineBreak;

implementation

end.
