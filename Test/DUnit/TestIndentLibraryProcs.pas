unit TestIndentLibraryProcs;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is TestIndentLibraryProcs
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2009 Anthony Steele.
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

{ AFS Jan 2009
  Test the "IndentLibraryProcs" flag
  and the IndentProcedureBody flag }

uses
  TestFrameWork,
  BaseTestProcess;

type
  TTestIndentLibraryProcs = class(TBaseTestProcess)
  private
    fbSaveIndentLibraryProcs: boolean;
    fbSaveIndentProcedureBody: boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestIndentTrueChange;
    procedure TestIndentTrueNoChange;

    procedure TestIndentFalseNoChange;
    procedure TestIndentFalseChange;

    procedure TestIndentProcedureBody;
    procedure TestIndentBoth;
  end;

implementation

uses
  { local }
  JcfStringUtils, JcfSettings;

const
  ProgramWithIndent: string =
    'program TestIndent;' + NativeLineBreak +
    NativeLineBreak +
    '  procedure AProc(r: Arec);' + NativeLineBreak +
    '  const' + NativeLineBreak +
    '    pi = 3.1415926535;' + NativeLineBreak +
    '  type' + NativeLineBreak +
    '    Brec = record' + NativeLineBreak +
    '      e, f, g, h: integer;' + NativeLineBreak +
    '    end;' + NativeLineBreak +
    '  var' + NativeLineBreak +
    '    Bee: brec;' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    r.Afield := ''Hah!'';' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    NativeLineBreak +
    'begin' + NativeLineBreak +
    'end.';

  ProgramNoIndent: string =
    'program TestIndent;' + NativeLineBreak +
    NativeLineBreak +
    'procedure AProc(r: Arec);' + NativeLineBreak +
    'const' + NativeLineBreak +
    '  pi = 3.1415926535;' + NativeLineBreak +
    'type' + NativeLineBreak +
    '  Brec = record' + NativeLineBreak +
    '    e, f, g, h: integer;' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    'var' + NativeLineBreak +
    '  Bee: brec;' + NativeLineBreak +
    'begin' + NativeLineBreak +
    '  r.Afield := ''Hah!'';' + NativeLineBreak +
    'end;' + NativeLineBreak +
    NativeLineBreak +
    'begin' + NativeLineBreak +
    'end.';

  ProgramBodyIndent: string =
    'program TestIndent;' + NativeLineBreak +
    NativeLineBreak +
    'procedure AProc(r: Arec);' + NativeLineBreak +
    '  const' + NativeLineBreak +
    '    pi = 3.1415926535;' + NativeLineBreak +
    '  type' + NativeLineBreak +
    '    Brec = record' + NativeLineBreak +
    '      e, f, g, h: integer;' + NativeLineBreak +
    '    end;' + NativeLineBreak +
    '  var' + NativeLineBreak +
    '    Bee: brec;' + NativeLineBreak +
    '  begin' + NativeLineBreak +
    '    r.Afield := ''Hah!'';' + NativeLineBreak +
    '  end;' + NativeLineBreak +
    NativeLineBreak +
    'begin' + NativeLineBreak +
    'end.';

  ProgramBothIndent: string =
    'program TestIndent;' + NativeLineBreak +
    NativeLineBreak +
    '  procedure AProc(r: Arec);' + NativeLineBreak +
    '    const' + NativeLineBreak +
    '      pi = 3.1415926535;' + NativeLineBreak +
    '    type' + NativeLineBreak +
    '      Brec = record' + NativeLineBreak +
    '        e, f, g, h: integer;' + NativeLineBreak +
    '      end;' + NativeLineBreak +
    '    var' + NativeLineBreak +
    '      Bee: brec;' + NativeLineBreak +
    '    begin' + NativeLineBreak +
    '      r.Afield := ''Hah!'';' + NativeLineBreak +
    '    end;' + NativeLineBreak +
    NativeLineBreak +
    'begin' + NativeLineBreak +
    'end.';

{ TTestIndentLibraryProcs }

procedure TTestIndentLibraryProcs.SetUp;
begin
  inherited;

  fbSaveIndentLibraryProcs := JcfFormatSettings.Indent.IndentLibraryProcs;
  fbSaveIndentProcedureBody := JcfFormatSettings.Indent.IndentProcedureBody;
end;

procedure TTestIndentLibraryProcs.TearDown;
begin
  inherited;

 JcfFormatSettings.Indent.IndentLibraryProcs := fbSaveIndentLibraryProcs;
 JcfFormatSettings.Indent.IndentProcedureBody := fbSaveIndentProcedureBody;
end;

procedure TTestIndentLibraryProcs.TestIndentTrueChange;
begin
  // when the setting is on, code without indent should be indented
 JcfFormatSettings.Indent.IndentLibraryProcs := True;
 JcfFormatSettings.Indent.IndentProcedureBody := False;

  TestFormatResult(ProgramNoIndent, ProgramWithIndent);
end;

procedure TTestIndentLibraryProcs.TestIndentTrueNoChange;
begin
  // when the setting is on, code with indent should be left as is
 JcfFormatSettings.Indent.IndentLibraryProcs := True;
 JcfFormatSettings.Indent.IndentProcedureBody := False;

  TestFormatResult(ProgramWithIndent, ProgramWithIndent);
end;

procedure TTestIndentLibraryProcs.TestIndentFalseNoChange;
begin
  // when the setting is off, code without indent should be left as is
 JcfFormatSettings.Indent.IndentLibraryProcs := False;
 JcfFormatSettings.Indent.IndentProcedureBody := False;

  TestFormatResult(ProgramNoIndent, ProgramNoIndent);
end;

procedure TTestIndentLibraryProcs.TestIndentFalseChange;
begin
  // when the setting is off, code with indent should have indent removed
 JcfFormatSettings.Indent.IndentLibraryProcs := False;
 JcfFormatSettings.Indent.IndentProcedureBody := False;

  TestFormatResult(ProgramWithIndent, ProgramNoIndent);
end;

procedure TTestIndentLibraryProcs.TestIndentProcedureBody;
begin
  // test the IndentProcedureBody setting
 JcfFormatSettings.Indent.IndentLibraryProcs := False;
 JcfFormatSettings.Indent.IndentProcedureBody := True;

  TestFormatResult(ProgramWithIndent, ProgramBodyIndent);
end;

procedure TTestIndentLibraryProcs.TestIndentBoth;
begin
  // test the IndentProcedureBody setting
 JcfFormatSettings.Indent.IndentLibraryProcs := True;
 JcfFormatSettings.Indent.IndentProcedureBody := True;

  TestFormatResult(ProgramWithIndent, ProgramBothIndent);
end;


initialization
  TestFramework.RegisterTest('Processes', TTestIndentLibraryProcs.Suite);

end.
